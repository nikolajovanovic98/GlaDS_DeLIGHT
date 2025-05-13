!------------------------------------------------------------------------------
! File: discharge.f90
! Written by: Nikola Jovanovic, 5 March 2025
! Modified by: -
! To compile: elmerf90 -o minelevmask.so minelevmask.F90
!------------------------------------------------------------------------------
SUBROUTINE minelevmask( Model, Solver, dt, Transient ) 
    USE DefUtils
    IMPLICIT NONE

    ! Import model components
    TYPE(Model_t)  :: Model 
    TYPE(Solver_t) :: Solver
    REAL(KIND=dp)  :: dt
    LOGICAL        :: Transient

    ! Load variables
    TYPE(Variable_t), POINTER   :: ubedVar
    INTEGER, POINTER            :: ubedPerm(:)
    REAL(KIND=dp), POINTER      :: ubedVals(:)

    INTEGER                     :: nn, totalNodes, terminusNode=-1, node
    REAL(KIND=dp)               :: bedElevationValue, min_ubed = 10000.
    CHARACTER(LEN=MAX_NAME_LEN) :: SolverName
    CHARACTER(LEN=20)           :: ierr_string


    ! To calculate
    INTEGER, POINTER            :: LowestElevMaskPerm(:)
    REAL(KIND=dp), POINTER      :: LowestElevMaskVals(:) 

    ! MPI Variables
    REAL(KIND=dp)               :: global_min_usurf
    INTEGER                     :: ierr

    !------------------------------------------------------------
    ! Starting the solver
    SolverName = "minelevmask"

    CALL Info(SolverName, "Masking the lowest elevation node.")

    ! Variable to solve 
    LowestElevMaskVals => Solver % Variable % Values
    LowestElevMaskPerm => Solver % Variable % Perm

    ! Get variables
    ubedVar => VariableGet(Model % Mesh % Variables, TRIM("ubed"))
    IF( ASSOCIATED( ubedVar ) ) THEN 
        ubedPerm => ubedVar % Perm
        ubedVals => ubedVar % Values
    ELSE
        CALL FATAL(SolverName, "Variable ubed not found")
    END IF 

    totalNodes = Model % Mesh % NumberOfNodes

    PRINT *, "total nodes = ", totalNodes
    DO nn=1, totalNodes 

            node = nn

            ! Access bed elev
            bedElevationValue = ubedVals(ubedPerm(node))

            ! Check if this node has the lowest bed elev
            IF (bedElevationValue < min_ubed) THEN
                min_ubed = bedElevationValue    ! Update lowest elevation
                !terminusNode = node             ! Mark this node as the terminus node
            END IF
    END DO

    PRINT *, "Min ubed = ", min_ubed

    IF (ParEnv % PEs > 1) THEN 
        CALL MPI_ALLREDUCE(min_ubed, global_min_usurf, 1, MPI_DOUBLE_PRECISION, MPI_MIN, ELMER_COMM_WORLD, ierr)
        IF (ierr /= 0) THEN 
            WRITE(ierr_string, '(I5)') ierr
            CALL FATAL(SolverName, "MPI_ALLREDUCE failed with error code " // TRIM(ierr_string))
        END IF
        min_ubed = global_min_usurf
    END IF


    DO nn=1, totalNodes 
        node = nn
        bedElevationValue = ubedVals(ubedPerm(node))
        IF (bedElevationValue == min_ubed) THEN

            LowestElevMaskVals(LowestElevMaskPerm(node)) = 1

        ELSE 

            LowestElevMaskVals(LowestElevMaskPerm(node)) = 0

        END IF 

    END DO 

    END SUBROUTINE minelevmask