!------------------------------------------------------------------------------
! File: new_flowrouting.f90
! Written by: Nikola Jovanovic, 25 April 2025
! Modified by: -
! To compile: elmerf90 -o new_flowrouting.so new_flowrouting.F90
!------------------------------------------------------------------------------
SUBROUTINE AdjustMoulinFlux( Model, Solver, dt, Transient )
    USE DefUtils
    IMPLICIT NONE

    ! Import model components 
    TYPE(Model_t)   :: Model
    TYPE(Solver_t)  :: Solver 
    REAL(KIND=dp)   :: dt 
    LOGICAL         :: Transient

    ! Params defined in the solver
    TYPE(ValueList_t), POINTER  :: SolverParams

    ! Initialise variables
    TYPE(Variable_t), POINTER   :: usurfVar, timeVar
    INTEGER, POINTER            :: usurfPerm(:)
    REAL(KIND=dp), POINTER      :: usurfVals(:), timeVals(:)

    INTEGER                     :: nn, node, ierr, totalNodes
    REAL(KIND=dp)               :: min_usurf = 10000., max_usurf = 1000., surfElevationValue, avg_usurf
    REAL(KIND=dp)               :: global_min_usurf, global_max_usurf
    REAL(KIND=dp)               :: DayOfYear, timeInDays, ElevDiffFactor
    REAL(KIND=dp)               :: seasonforc, diurnal, wintermask, zeromelt
    REAL(KIND=dp)               :: yearinsec = 365.0*24.*60.*60.
    REAL(KIND=dp)               :: GaussianPulse, PulseAmplitude, PulseCentre, PulseWidth
    REAL(KIND=dp)               :: randomNoise, r, r1, finalMelt
    CHARACTER(LEN=MAX_NAME_LEN) :: SolverName
    CHARACTER(LEN=20)           :: ierr_string

    ! Variables to solve 
    INTEGER, POINTER            :: ScaledMoulinFluxPerm(:)
    REAL(KIND=dp), POINTER      :: ScaledMoulinFluxVals(:)




    ! Starting the solver 
    SolverName = "AdjustMoulinFlux"

    CALL Info(SolverName, "Starting moulin flux calculation", level=4)


    ! Get solver parameters 
    SolverParams => GetSolverParams()

    ! Main code
    ! ---------------------------------------------------------------------------------------------------------------------
    ! Variables to solve
    ScaledMoulinFluxVals => Solver % Variable % Values
    ScaledMoulinFluxPerm => Solver % Variable % Perm

    usurfVar => VariableGet(Model % Mesh % Variables, TRIM("usurf"))
    IF( ASSOCIATED( usurfVar ) ) THEN 
        usurfPerm => usurfVar % Perm
        usurfVals => usurfVar % Values
    ELSE
        CALL FATAL(SolverName, "Variable ubed not found")
    END IF 

    timeVar => VariableGet(Model % Mesh % Variables, "Time")
    IF ( ASSOCIATED( timeVar ) ) THEN 
        !timePerm => timeVar % Perm
        timeVals => timeVar % Values
    ELSE
        CALL FATAL(SolverName, "Variable Time not found")
    END IF 

    totalNodes = Model % Mesh % NumberOfNodes

    DO nn=1, totalNodes

        node = nn 

        ! Access surface elev. 
        surfElevationValue = usurfVals(usurfPerm(node))

        ! Find lowest elev. 
        IF (surfElevationValue < min_usurf) THEN 
            min_usurf = surfElevationValue

        ! Find highest elev.
        ELSE IF (surfElevationValue > max_usurf) THEN 
            max_usurf = surfElevationValue

        END IF 

    END DO 

    IF (ParEnv % PEs > 1) THEN 

        CALL MPI_ALLREDUCE(min_usurf, global_min_usurf, 1, MPI_DOUBLE_PRECISION, MPI_MIN, ELMER_COMM_WORLD, ierr)
        IF (ierr /= 0) THEN 
                
            WRITE(ierr_string, '(I5)') ierr
            CALL FATAL(SolverName, "MPI_ALLREDUCE failed with error code " // TRIM(ierr_string))

        END IF 

        CALL MPI_ALLREDUCE(max_usurf, global_max_usurf, 1, MPI_DOUBLE_PRECISION, MPI_MAX, ELMER_COMM_WORLD, ierr)
        IF (ierr /= 0) THEN 
                
            WRITE(ierr_string, '(I5)') ierr
            CALL FATAL(SolverName, "MPI_ALLREDUCE failed with error code " // TRIM(ierr_string))

        END IF 
           min_usurf = global_min_usurf
           max_usurf = global_max_usurf
    END IF

    ! -----------------------------------------------------------
    ! ---------------- FORCING SECTION --------------------------
    ! -----------------------------------------------------------

    ! Convert current time (in years) to day of year
    timeInDays = MOD(timeVals(1), 1.0)*365.0
    DayOfYear = timeInDays

    ! Seasonal forcing
    ! -----------------------------------------------------------

    seasonforc = 0.5*sin(2*pi*((DayOfYear-180)/365)+1.0)+0.5
    diurnal    = 0.5*sin(2*pi*DayOfYear-(pi/2))+0.5

    IF (DayOfYear >= 59.0 .AND. DayOfYear <= 334.0) THEN 
        wintermask = 1.0
    ELSE
        wintermask = 0.0
    END IF 

    ! Seasonal forcing with zero melt in winter 
    ! ----------------------------------------------------------

    zeromelt = seasonforc*wintermask

    ! Gaussian pulse
    ! ----------------------------------------------------------
    !PulseCentre    = 213.   ! August 1 
    !PulseAmplitude = 1.5    
    !PulseWidth     = 20.  
    !Phase          = ((DayOfYear - PulseCentre + 1))
    !GaussianPulse  = PulseAmplitude*EXP(-((DayOfYear-PulseCentre)**2) / (2*PulseWidth)**2)

    ! Random noise
    ! -----------------------------------------------------------
    !CALL RANDOM_NUMBER(r)
    !randomNoise = 0.2*(r-0.5)         ! Random noise between -0.1 and 0.1

    finalMelt = seasonforc + randomNoise

    IF (finalMelt < 0 ) THEN 
        finalMelt = 0 
    ELSE IF (finalMelt > 0 ) THEN 
        finalMelt = 1
    END IF

    ! Completely random 
    ! ------------------------------------------------------------
    CALL RANDOM_NUMBER(r1)
    

    ! Calculate average surface elev
    avg_usurf = (max_usurf + min_usurf) / 2.0 

    DO nn=1, SIZE(ScaledMoulinFluxVals)

        node = nn 

        ! Access surface elev. 
        surfElevationValue = usurfVals(usurfPerm(node))

        ! Calculate ElevDiff Factor
        ElevDiffFactor = (avg_usurf - surfElevationValue) / (max_usurf - min_usurf + 1.0d-6)
        
        ! Calculate Moulin Flux
        ScaledMoulinFluxVals(ScaledMoulinFluxPerm(node)) = 0.009*yearinsec*(1.+ ElevDiffFactor)*seasonforc*diurnal

    END DO 

END SUBROUTINE AdjustMoulinFlux
