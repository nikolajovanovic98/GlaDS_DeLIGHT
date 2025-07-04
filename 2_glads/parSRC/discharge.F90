!------------------------------------------------------------------------------
! File: discharge.f90
! Written by: Nikola Jovanovic, 5 February 2025
! Modified by: -
! To compile: elmerf90 -o discharge.so discharge.F90
!------------------------------------------------------------------------------
SUBROUTINE get_Discharge( Model, Solver, dt, Transient ) 
    USE DefUtils
    IMPLICIT NONE

    ! Import model components
    TYPE(Model_t)  :: Model 
    TYPE(Solver_t) :: Solver
    REAL(KIND=dp)  :: dt
    LOGICAL        :: Transient

    ! Params defined in the Solver
    TYPE(ValueList_t), POINTER  :: SolverParams

    ! Initialise variables 
    TYPE(Element_t), POINTER    :: Edge
    TYPE(Variable_t), POINTER   :: SheetVar, SheetDisVar, ChannelVar, MaskVar
    INTEGER, POINTER            :: SheetPerm(:), SheetDisPerm(:), ChannelPerm(:), MaskPerm(:)
    REAL(KIND=dp), POINTER      :: SheetVals(:), SheetDisVals(:), ChannelVals(:), MaskVals(:)

    INTEGER                     :: nn, ee, numNodes, terminusNode
    CHARACTER(LEN=MAX_NAME_LEN) :: SolverName

    ! Values to calculate
    REAL(KIND=dp)               :: volFluxSheet, volFluxChannel, SheetDisMag

    ! Variables to calculate
    TYPE(Variable_t), POINTER   :: terminusSheetVar, terminusChannelVar 
    INTEGER, POINTER            :: terminusSheetPerm(:), terminusChannelPerm(:)
    REAL(KIND=dp), POINTER      :: terminusSheetVals(:), terminusChannelVals(:)

    INTEGER, POINTER            :: TerminusFluxPerm(:)
    REAL(KIND=dp), POINTER      :: TerminusFluxVals(:)


    ! Starting the Solver
    SolverName = "get_Discharge"

    CALL Info(SolverName, "Starting subglacial outflow calculation", Level=4)

    ! Get solver parameters
    SolverParams => GetSolverParams()



    ! Main code
    ! ---------------------------------------------------------------------------------------------------------------------
    ! Variables to solve
    TerminusFluxVals => Solver % Variable % Values
    TerminusFluxPerm => Solver % Variable % Perm

    ! Variables containing the GlaDS sheet thickness and discharge and channel flux 

    SheetVar => VariableGet(Model % Mesh % Variables, 'Sheet Thickness')
    IF ( ASSOCIATED( SheetVar) ) THEN
        SheetPerm => SheetVar % Perm
        SheetVals => SheetVar % Values
    ELSE
        CALL FATAL(SolverName, 'Variable Sheet Thickness not found')
    END IF

    SheetDisVar => VariableGet(Model % Mesh % Variables, 'Sheet Discharge')
    IF ( ASSOCIATED( SheetDisVar) ) THEN
        SheetDisPerm => SheetDisVar % Perm
        SheetDisVals => SheetDisVar % Values
    ELSE
        CALL FATAL(SolverName, 'Variable Sheet Discharge not found')
    END IF

    ChannelVar => VariableGet(Model % Mesh % Variables, 'Channel Flux')
    IF ( ASSOCIATED( ChannelVar) ) THEN
        ChannelPerm => ChannelVar % Perm
        ChannelVals => ChannelVar % Values
    ELSE
        CALL FATAL(SolverName, 'Variable Channel Flux not found')
    END IF

    MaskVar => VariableGet(Model % Mesh % Variables, 'LowestElevationMask')
    IF ( ASSOCIATED (MaskVar)) THEN 
        MaskPerm => MaskVar % Perm 
        MaskVals => MaskVar % Values
    ELSE
        CALL FATAL(SolverName, 'Variable LowestElevationMask not found')
    END IF

    ! Loop over all nodes
    numNodes = Solver % Mesh % Nodes % NumberOfNodes 

    terminusNode    = -1

    DO nn=1, numNodes
        
        IF (MaskVals(MaskPerm(nn)) == 1) THEN 

            terminusNode = nn

            ! initalise
            volFluxSheet    = 0
            volFluxChannel  = 0
            
            ! Sheet discharge magnitude
            SheetDisMag = ( SheetDisVals(2*(SheetDisPerm(terminusNode)-1) + 1)**2.0 +   &
                            SheetDisVals(2*(SheetDisPerm(terminusNode)-1) + 2)**2.0 )**0.5

            ! Volume flux from the sheet is given by multiplying sheet discharge and sheet thk
            volFluxSheet = SheetVals(SheetPerm(terminusNode)) * SheetDisMag

            ! Loop over all edges
            DO ee=1, Solver % Mesh % NumberOfEdges

                Edge => Solver % Mesh % Edges(ee)

                IF  (Edge % NodeIndexes(1).EQ.terminusNode .OR. Edge % NodeIndexes(2).EQ.terminusNode) THEN 

                    ! Halve values for edges at partition boundaries because these
                    ! will be counted twice 
                    IF (Solver % Mesh % ParallelInfo % EdgeInterface(ee)) THEN
                        volFluxChannel = volFluxChannel + 0.5*ChannelVals(ChannelPerm(numNodes + ee))
                    ELSE
                        volFluxChannel = volFluxChannel + ChannelVals(ChannelPerm(numNodes + ee))
                    END IF
                        
                END IF

            END DO

            TerminusFluxVals(TerminusFluxPerm(terminusNode))       = volFluxChannel + volFluxSheet
            PRINT *, "Volume flux from the sheet is: ", volFluxSheet
            PRINT *, "Volume flux from the channels is: ", volFluxChannel
            PRINT *, "Volume flux across terminus is: ", TerminusFluxVals(TerminusFluxPerm(terminusNode))
        END IF  
    END DO

END SUBROUTINE get_Discharge
