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

    ! To calculate
    INTEGER, POINTER            :: LowestElevMaskPerm(:)
    REAL(KIND=dp), POINTER      :: LowestElevMaskVals(:) 

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

    DO nn=1, totalNodes 

            node = nn

            ! Access bed elev
            bedElevationValue = ubedVals(ubedPerm(node))

            ! Check if this node has the lowest bed elev
            IF (bedElevationValue < min_ubed) THEN
                min_ubed = bedElevationValue    ! Update lowest elevation
                terminusNode = node             ! Mark this node as the terminus node
            END IF
    END DO

    DO nn=1, totalNodes 
        node = nn
        IF (node == terminusNode) THEN

            LowestElevMaskVals(LowestElevMaskPerm(node)) = 1

        ELSE 

            LowestElevMaskVals(LowestElevMaskPerm(node)) = 0

        END IF 

    END DO 

    END SUBROUTINE minelevmask