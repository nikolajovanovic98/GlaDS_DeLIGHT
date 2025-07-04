!------------------------------------------------------------------------------
! File: minelevmask.f90
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
    TYPE(Variable_t), POINTER   :: ubedVar, usurfVar
    INTEGER, POINTER            :: ubedPerm(:), usurfPerm(:)
    REAL(KIND=dp), POINTER      :: ubedVals(:), usurfVals(:)

    TYPE(Mesh_t), POINTER       :: Mesh
    TYPE(Element_t), POINTER    :: Element

    INTEGER                     :: nn, totalNodes, node, t, j, n, bc_id, nodeid
    REAL(KIND=dp)               :: bedElevationValue, min_ubed = 10000., surfElevationValue, min_usurf = 10000.
    CHARACTER(LEN=MAX_NAME_LEN) :: SolverName
    CHARACTER(LEN=20)           :: ierr_string


    ! To calculate
    INTEGER, POINTER            :: LowestElevMaskPerm(:)
    REAL(KIND=dp), POINTER      :: LowestElevMaskVals(:) 

    ! MPI Variables
    REAL(KIND=dp)               :: global_min_usurf, global_min_ubed
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

    usurfVar => VariableGet(Model % Mesh % Variables, TRIM("usurf"))
    IF( ASSOCIATED( usurfVar ) ) THEN 
        usurfPerm => usurfVar % Perm
        usurfVals => usurfVar % Values
    ELSE
        CALL FATAL(SolverName, "Variable usurf not found")
    END IF 
    
    
    totalNodes = Model % Mesh % NumberOfNodes

    ! loop over boundary elements
    DO t=1, Model % Mesh % NumberOfBoundaryElements

        ! get element information
        Element => GetBoundaryElement(t)

        bc_id = GetBCId(Element)

        ! access edge BC
        IF (bc_id /= 1) CYCLE

        n = GetElementNOFNodes()

        ! loop over element nodes
        DO j=1, n
            
            ! get node indices
            nodeid = Element % NodeIndexes(j)

            ! get surf and bed elev values
            bedElevationValue = ubedVals(ubedPerm(nodeid))
            surfElevationValue = usurfVals(usurfPerm(nodeid))

            ! min ubed logic
            IF (bedElevationValue < min_ubed) THEN 

                min_ubed = bedElevationValue

            END IF 

            ! min usurf logic
            IF (surfElevationValue < min_usurf) THEN 

                min_usurf = surfElevationValue
            
            END IF 
        
        END DO
    END DO

    ! get global lowest elev for partitioned mesh
    IF (ParEnv % PEs > 1) THEN 
        CALL MPI_ALLREDUCE(min_ubed, global_min_ubed, 1, MPI_DOUBLE_PRECISION, MPI_MIN, ELMER_COMM_WORLD, ierr)
        CALL MPI_ALLREDUCE(min_usurf, global_min_usurf, 1, MPI_DOUBLE_PRECISION, MPI_MIN, ELMER_COMM_WORLD, ierr)

        IF (ierr /= 0) THEN 
            WRITE(ierr_string, '(I5)') ierr
            CALL FATAL(SolverName, "MPI_ALLREDUCE failed with error code " // TRIM(ierr_string))
        END IF
        min_ubed = global_min_ubed
        min_usurf = global_min_usurf
    END IF

    ! get lowest elevation mask
    DO nn=1, totalNodes 
        node = nn
        bedElevationValue = ubedVals(ubedPerm(node))
        surfElevationValue = usurfVals(usurfPerm(node))
        IF (surfElevationValue == min_usurf) THEN 

            LowestElevMaskVals(LowestElevMaskPerm(node)) = 1

        ELSE 

            LowestElevMaskVals(LowestElevMaskPerm(node)) = 0

        END IF 

    END DO 

    END SUBROUTINE minelevmask  