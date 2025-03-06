!------------------------------------------------------------------------------
! File: minHydPot.F90
! Written by: Nikola Jovanovic, 25 February 2025
! Modified by: -
! To compile: elmerf90 -o minHydPot.so minHydPot.F90
!------------------------------------------------------------------------------

FUNCTION AdjustMoulinFlux( Model, nodenumber, usurf) RESULT(ScaledMoulinFlux)
    USE DefUtils
    IMPLICIT NONE

    ! Import SIF components
    TYPE(Model_t)  :: Model
    INTEGER        :: nodenumber
    REAL(KIND=dp)  :: usurf
    
    ! Initialise values to
    REAL(KIND=dp)               :: min_usurf = 10000., max_usurf = 1000., surfElevationValue, avg_usurf
    REAL(KIND=dp)               :: ElevDiffFactor, yearinsec = 365.0*24.*60.*60.
    INTEGER                     :: min_node = -1, totalNodes = -1, nn, node, terminusNode, highestNode
    LOGICAL                     :: firstPass = .TRUE. 
    CHARACTER(LEN=MAX_NAME_LEN) :: FunctionName

    ! Initialise variables
    TYPE(Variable_t), POINTER   :: usurfVar, QmVar
    REAL(KIND=dp), POINTER      :: usurfVals(:), QmVals(:)
    INTEGER, POINTER            :: usurfPerm(:), QmPerm(:)

    ! Values to calculate 
    REAL(KIND=dp)               :: ScaledMoulinFlux

    SAVE terminusNode, min_usurf, highestNode, max_usurf, firstPass


    !------------------------------------------------------------
    ! Starting the function
    FunctionName = "AdjustMoulinFlux"

    usurfVar => VariableGet(Model % Mesh % Variables, TRIM("usurf"))
    IF( ASSOCIATED( usurfVar ) ) THEN 
        usurfPerm => usurfVar % Perm
        usurfVals => usurfVar % Values
    ELSE
        CALL FATAL(FunctionName, "Variable ubed not found")
    END IF 

    !QmVar => VariableGet(Model % Mesh % Variables, "Moulin Flux")
    !IF ( ASSOCIATED( QmVar ) ) THEN 
    !    QmPerm => QmVar % Perm
    !    QmVals => QmVar % Values
    !ELSE
    !    CALL FATAL(FunctionName, "Variable Moulin Flux not found")
    !END IF 

    totalNodes = Model % Mesh % NumberOfNodes

    IF (firstPass) THEN
        firstPass = .False. 

        DO nn=1, totalNodes 

                node = nn

                ! Access bed elev
                surfElevationValue = usurfVals(usurfPerm(node))

                ! Check if this node has the lowest bed elev
                IF (surfElevationValue < min_usurf) THEN
                    min_usurf = surfElevationValue    ! Update lowest elevation
                    terminusNode = node               ! Mark this node as the terminus node
                
                ELSE IF (surfElevationValue > max_usurf) THEN 
                    max_usurf = surfElevationValue
                    highestNode = node

                END IF
        END DO

    END IF

    avg_usurf = (max_usurf + min_usurf) / 2.0 

    ElevDiffFactor = (avg_usurf - usurf) / (max_usurf - min_usurf + 1.0d-6)

    ScaledMoulinFlux = 0.9*yearinsec*(1.+ ElevDiffFactor)
    
    RETURN
    
END FUNCTION AdjustMoulinFlux