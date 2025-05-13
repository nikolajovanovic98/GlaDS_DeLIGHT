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
    TYPE(Element_t), POINTER    :: Edge ! Element
    TYPE(Variable_t), POINTER   :: SheetVar, SheetDisVar, ChannelVar, ZbVar, HydPotVar
    INTEGER, POINTER            :: SheetPerm(:), SheetDisPerm(:), ChannelPerm(:), ZbPerm(:), HydPotPerm(:)
    REAL(KIND=dp), POINTER      :: SheetVals(:), SheetDisVals(:), ChannelVals(:), ZbVals(:), HydPotVals(:)

    INTEGER                     :: nn, ee, numNodes, terminusNode, node, ierr, localterminus
    CHARACTER(LEN=MAX_NAME_LEN) :: SolverName
    CHARACTER(LEN=20)           :: ierr_string

    LOGICAL                     :: GotIt

    ! Values to calculate
    REAL(KIND=dp)               :: lowestElev, bedElevationValue, globalElev
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

    !terminusSheetVar => VariableGet(Model % Mesh % Variables, 'Sheet Outflow')
    !IF ( ASSOCIATED(terminusSheetVar)) THEN 
    !    terminusSheetPerm => terminusSheetVar % Perm
    !    terminusSheetVals => terminusSheetVar % Values
    !ELSE
    !    CALL FATAL(SolverName, 'Variable Sheet Outflow not found')
    !END IF 

    !terminusChannelVar => VariableGet(Model % Mesh % Variables, 'Channel Outflow')
    !IF ( ASSOCIATED(terminusChannelVar )) THEN 
    !    terminusChannelPerm => terminusChannelVar % Perm 
    !    terminusChannelVals => terminusChannelVar % Values
    !ELSE 
    !    CALL FATAL(SolverName, 'Variable Channel Outflow not found')
    !END IF 
    
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
        SheetPerm    => SheetDisVar % Perm
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

    ZbVar => VariableGet(Model % Mesh % Variables, TRIM('ubed'))
    IF ( ASSOCIATED( ZbVar) ) THEN
        ZbPerm => ZbVar % Perm
        ZbVals => ZbVar % Values
        !PRINT *, ZbVals
    ELSE
        CALL FATAL(SolverName, 'Variable Zb not found')
    END IF

    HydPotVar => VariableGet(Model % Mesh % Variables, 'Hydraulic Potential')
    IF ( ASSOCIATED (HydPotVar)) THEN 
        HydPotPerm => HydPotVar % Perm 
        HydPotVals => HydPotVar % Values
    ELSE
        CALL FATAL(SolverName, 'Variable Hydraulic Potential not found')
    END IF

    ! Loop over all nodes
    numNodes = Solver % Mesh % Nodes % NumberOfNodes 

    terminusNode   = -1
    lowestElev     = 10000. 
    volFluxSheet   = 0
    volFluxChannel = 0


    DO ee=1, Solver % Mesh % NumberOfEdges 
        Edge => Solver % Mesh % Edges(ee)

        ! Loop through all nodes associated with the current edge
        DO nn=1, SIZE(Edge % NodeIndexes)

            node = Edge % NodeIndexes(nn)

            ! Access the permuted bed elevation value
            bedElevationValue = ZbVals(ZbPerm(node))
            !PRINT *, bedElevationValue

            ! Check if this node has the lowest bed elevation
            IF (bedElevationValue < lowestElev) THEN
                lowestElev = bedElevationValue      ! Update lowest elevation
                terminusNode = node                 ! Mark this node as the terminus node
            END IF
        END DO
    END DO

    PRINT *, "sheetdisperm: ", SheetDisPerm(terminusNode)

    ! Parallel run 
    IF (ParEnv % PEs > 1) THEN 
        CALL MPI_ALLREDUCE(lowestElev, globalElev, 1, MPI_DOUBLE_PRECISION, MPI_MIN, ELMER_COMM_WORLD, ierr)
        
        IF (ierr /= 0) THEN 
                
            WRITE(ierr_string, '(I5)') ierr
            CALL FATAL(SolverName, "MPI_ALLREDUCE failed with error code " // TRIM(ierr_string))

        END IF 

        PRINT *, "Global elevation: ", globalElev
        
        IF (globalElev == lowestElev) THEN 

            PRINT *, "Lowest elev :", lowestElev
            PRINT *, "ParEnv % PEs:", ParEnv % PEs
            PRINT *, "terminusNode:", terminusNode
            PRINT *, "SheetDisPerm(terminusNode):", SheetDisPerm(terminusNode)
            !PRINT *, "Accessing SheetDisVals at index:", 2*(SheetDisPerm(terminusNode)-1) + 1
            !PRINT *, "Accessing ChannelVals at index:", numNodes + ee

            SheetDisMag = ( SheetDisVals(2*(SheetDisPerm(terminusNode)-1) + 1)**2.0 +   &
                            SheetDisVals(2*(SheetDisPerm(terminusNode)-1) + 2)**2.0 )**0.5

            ! Volume flux from the sheet is given by multiplying sheet discharge and sheet thk
            volFluxSheet = SheetVals(SheetPerm(terminusNode)) * SheetDisMag

            DO ee=1, Solver % Mesh % NumberOfEdges

                Edge => Solver % Mesh % Edges(ee)

                IF  (Edge % NodeIndexes(1).EQ.terminusNode .OR. Edge % NodeIndexes(2).EQ.terminusNode) THEN 

                    ! Halve values for edges at partition boundaries bevause these
                    ! will be counted twice 
                    IF (Solver % Mesh % ParallelInfo % EdgeInterface(ee)) THEN
                        volFluxChannel = volFluxChannel + 0.5*ChannelVals(ChannelPerm(numNodes + ee))
                    ELSE
                        volFluxChannel = volFluxChannel + ChannelVals(ChannelPerm(numNodes + ee))
                    END IF
                        
                END IF

            END DO

            TerminusFluxVals(TerminusFluxPerm(terminusNode))       = volFluxChannel + volFluxSheet
            PRINT *, "Elevation is: ", lowestElev
            !PRINT *, "Volume flux from the sheet is: ", volFluxSheet
            PRINT *, "Volume flux from the channels is: ", volFluxChannel
            PRINT *, "Volume flux across terminus is: ", TerminusFluxVals(TerminusFluxPerm(terminusNode))
        END IF 
    END IF 
    
    IF (ParEnv % PEs == 1) THEN 
        IF (terminusNode /= -1) THEN 

            !PRINT *, "terminusNode:", terminusNode

            !PRINT *, "SheeDis: ", SheetDisPerm(terminusNode)
            ! This is the calculation if the Sheet Discharge is always a 2D vector. Does it work here?
            ! Sometimes it gives a segmentation fault.
            SheetDisMag = ( SheetDisVals(2*(SheetDisPerm(terminusNode)-1) + 1)**2.0 +   &
                            SheetDisVals(2*(SheetDisPerm(terminusNode)-1) + 2)**2.0 )**0.5

            ! Volume flux from the sheet is given by multiplying sheet discharge and sheet thk
            volFluxSheet = SheetVals(SheetPerm(terminusNode)) * SheetDisMag

            DO ee=1, Solver % Mesh % NumberOfEdges

                Edge => Solver % Mesh % Edges(ee)

                IF  (Edge % NodeIndexes(1).EQ.terminusNode .OR. Edge % NodeIndexes(2).EQ.terminusNode) THEN 
            
                    !IF (Solver % Mesh % ParallelInfo % EdgeInterface(ee)) THEN
                    !IF (ParEnv % PEs > 1) THEN 
                        ! Halve values for edges at partition boundaries bevause these
                        ! will be counted twice 
                    !    volFluxChannel = volFluxChannel + 0.5*ChannelVals(ChannelPerm(numNodes + ee))
                    !ELSE 
                        volFluxChannel = volFluxChannel + ChannelVals(ChannelPerm(numNodes + ee))
                    
                    !END IF

                END IF

            END DO

        END IF
    END IF
    
    !terminusSheetVals(terminusSheetPerm(terminusNode))     = volFluxSheet
    !terminusChannelVals(terminusChannelPerm(terminusNode)) = volFluxChannel
    
    !                                                          terminusSheetVals(terminusSheetPerm(terminusNode)) + &
    !                                                         terminusChannelVals(terminusChannelPerm(terminusNode))
    !PRINT *, "Terminus node is: ", localterminus
    !PRINT *, "Elevation is: ", lowestElev
    !PRINT *, "Volume flux from the sheet is: ", volFluxSheet
    !PRINT *, "Volume flux from the channels is: ", volFluxChannel
    !PRINT *, "Volume flux across terminus is: ", TerminusFluxVals(TerminusFluxPerm(terminusNode))

END SUBROUTINE get_Discharge
