
C
C     ***********************************************************************************************************
C                             ABAQUS USER ELEMENT SUBROUTINE (UEL)
C                     --- FOR FASTENER-BASED SHEAR WALL/DIAPHRAGM ANALYSIS  ---
C              -----------------------------------------------------------------------
C                                                             AUTHOR:  Chu Ding                   (chud91@vt.edu)
C                                                             ADVISOR: Dr. Cristopher D. Moen     (cmoen@vt.edu)
C                                                             RESEARCH GROUP: http://www.moen.cee.vt.edu/
C     ***********************************************************************************************************
C
      SUBROUTINE UEL(RHS,AMATRX,SVARS,ENERGY,NDOFEL,NRHS,NSVARS,
     +     PROPS,NPROPS,COORDS,MCRD,NNODE,U,DU,V,A,JTYPE,TIME,DTIME,
     +     KSTEP,KINC,JELEM,PARAMS,NDLOAD,JDLTYP,ADLMAG,PREDEF,
     +     NPREDF,LFLAGS,MLVARX,DDLMAG,MDLOAD,PNEWDT,JPROPS,NJPROP,
     +     PERIOD)
C
C
C     *****************************************************************
C     
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0, TWO = 2.D0)
C
C
      DIMENSION RHS(MLVARX,*),AMATRX(NDOFEL,NDOFEL),PROPS(*),
     1 SVARS(*),ENERGY(8),COORDS(MCRD,NNODE),U(NDOFEL),
     2 DU(MLVARX,*),V(NDOFEL),A(NDOFEL),TIME(2),PARAMS(*),
     3 JDLTYP(MDLOAD,*),ADLMAG(MDLOAD,*),DDLMAG(MDLOAD,*),
     4 PREDEF(2,NPREDF,NNODE),LFLAGS(*),JPROPS(*)
C
      DIMENSION SRESID(4)
      DIMENSION SPR_AMATRX(4,4), SPR_SRESID(4)
C
      DOUBLE PRECISION SPR_LEN, SPR_DISP
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
      DOUBLE PRECISION SPR_DISP_X, SPR_DISP_Y
      DOUBLE PRECISION SPR_K, SPR_F
      DOUBLE PRECISION SPR_SGN
      DOUBLE PRECISION SPR_K_X, SPR_K_Y
      DOUBLE PRECISION SPR_F_X, SPR_F_Y
      DOUBLE PRECISION SPR_DISP_U, SPR_DISP_V
      DOUBLE PRECISION SPR_K_U, SPR_K_V
      DOUBLE PRECISION SPR_F_U, SPR_F_V
C
      DOUBLE PRECISION SPR_K_SEC, SPR_K_SEC_X, SPR_K_SEC_Y
      DOUBLE PRECISION SPR_K_SEC_U, SPR_K_SEC_V
C
      DOUBLE PRECISION SPR_ORIENT_1, SPR_ORIENT_2
C
      DOUBLE PRECISION SPR_DISP1, SPR_DISP2, SPR_F1, SPR_F2, SPR_ENERGY
C
      DOUBLE PRECISION SPR_YD
C
      INTEGER I_SPR_NUM
      INTEGER KPNT, KSEC, KORIENT
C
      CHARACTER FILENAME*200
      CHARACTER*(*) FILEPATH
C
C
C     --------------                  Choose spring data output path                  --------------
C     **********************************************************************************************
      PARAMETER (FILEPATH = '/home/cdmoen/job/r0/uel_output/')
C     **********************************************************************************************
C
C
C     --------------                  Select your spring type                         --------------
C     **********************************************************************************************
C     * 1: Radial spring, 2: Coupled spring pair, 3: Uncoupled spring pair, 4: Oriented spring pair
C     * 5: Modified radial spring
      KPNT = 3
C     * 0: Tangent stiffness, 1: Secant stiffness
      KSEC = 0
C     * 0: Default deformation quadrants, 1: Displacement-based deformation quadrants        
      KORIENT = 1
C     * 0:  Forbid spring data output, 1: Permit spring data output
      KOUTPUT = 1
C     **********************************************************************************************
C
C
C     Specify fastener mass
      AM   = 4.5D-5
C
C     Initialize vector variables
      DO K1 = 1, NDOFEL
          SRESID(K1) = ZERO
          SPR_SRESID(K1) = ZERO
          DO KRHS = 1, NRHS
              RHS(K1,KRHS) = ZERO
          END DO
          DO K2 = 1, NDOFEL
              AMATRX(K1, K2) = ZERO
              SPR_AMATRX(K1,K2) = ZERO
          END DO
      END DO
C
C     Initialize scalar variables
      SPR_DISP_X = ZERO
      SPR_DISP_Y = ZERO
      SPR_K_X = ZERO
      SPR_K_Y = ZERO
      SPR_F_X = ZERO
      SPR_F_Y = ZERO
C
C
C     **************** Generate spring geometry info. ****************
      IF (KPNT .EQ. 1) THEN
C     * Radial-spring model
          CALL SGEOM(U,COORDS,SPR_LEN,SPR_DISP,SPR_COS_X,SPR_COS_Y,SPR_DISP_X,SPR_DISP_Y,PROPS,SVARS)
      ELSE IF (KPNT. EQ. 2) THEN
C     * Coupled-spring model
          CALL SGEOM_CUP(U,SPR_DISP,SPR_COS_X,SPR_COS_Y,SPR_DISP_X,SPR_DISP_Y)
      ELSE IF (KPNT .EQ. 4) THEN
C     * Oriented spring pair
          CALL SGEOM_ORN(U, SPR_DISP_X, SPR_DISP_Y, SPR_COS_X, SPR_COS_Y, SPR_DISP_U, SPR_DISP_V, SVARS)
      ELSE IF (KPNT .EQ. 5) THEN
          IF (SVARS(180) .EQ. ZERO) THEN
              CALL SGEOM(U,COORDS,SPR_LEN,SPR_DISP,SPR_COS_X,SPR_COS_Y,SPR_DISP_X,SPR_DISP_Y,PROPS,SVARS)
          ELSE 
              CALL SGEOM_MDRD(U,COORDS,SPR_LEN,SPR_DISP,SPR_COS_X,SPR_COS_Y,SPR_DISP_X,SPR_DISP_Y,PROPS,SVARS)
          END IF
      ELSE
C     * 2-spring model
          CALL SGEOM(U,COORDS,SPR_LEN,SPR_DISP,SPR_COS_X,SPR_COS_Y,SPR_DISP_X,SPR_DISP_Y,PROPS,SVARS)
      END IF
C
C  
C     **************** Record connection yielding ****************  
      IF (KPNT .EQ. 1 .OR. KPNT .EQ. 2 .OR. KPNT. EQ. 5) THEN
          IF (SVARS(180) .EQ. ZERO) THEN
              IF (DABS(SPR_DISP) .GE. DABS(PROPS(1))) THEN
                  SPR_YD = ONE
                  SVARS(180) = ONE
                  SVARS(181) = SPR_COS_X
                  SVARS(182) = SPR_COS_Y
              ELSE 
                  SPR_YD = ZERO
                  SVARS(180) = ZERO
              END IF
          END IF
      END IF 
C
C
C     **************** Save spring original orientation ****************
*     * Set up spring positive/negative deformation rule
C     * Only applicable to radial spring/coupled spring/modified radial spring
      IF (KPNT .EQ. 1 .OR. KPNT .EQ. 2 .OR. KPNT .EQ. 5) THEN
C         * Deformation quadrant based on initial displacement
          IF (KORIENT .EQ. 1) THEN
              IF (SVARS(128) .NE. ONE) THEN
C                 * Use default deformation quadrant when deformation is "zero"
                  IF (DABS(SPR_DISP) .LE. TWO * TOL) THEN
                      SVARS(129) = 1.D0/DSQRT(2.D0)
                      SVARS(130) = 1.D0/DSQRT(2.D0)
                  ELSE 
C                 * Now use displacement-based quadrant
C                     * Save spring orientation
                      SVARS(129) = SPR_COS_X
                      SVARS(130) = SPR_COS_Y
C                     * Keep spring orientation quadrant fixed now
                      SVARS(128) = SVARS(128) + ONE        
                  END IF
              END IF
C             * Retrieve spring orientation
              SPR_ORIENT_1 = SVARS(129)
              SPR_ORIENT_2 = SVARS(130)
C         * Default deformation quadrant
          ELSE
              SPR_ORIENT_1 = 1.D0/DSQRT(2.D0)
              SPR_ORIENT_2 = 1.D0/DSQRT(2.D0)
          END IF
      END IF 
C
C
C     **************** Adjust spring deformation sign ****************
C     * Only applicable to radial spring/coupled spring model
      IF (KPNT .EQ. 1 .OR. KPNT .EQ. 2 .OR. (KPNT .EQ. 5 .AND. SVARS(180) .EQ. ZERO)) THEN
          SPR_SGN = ONE
C         * For the case of "real compression"
          IF (SPR_DISP .LT. ZERO) THEN
              SPR_SGN = ONE
C         * For the case of "fake compression" determined by deformation quadrants
          ELSE IF (SPR_DISP_X .NE. ZERO .AND. SPR_DISP_Y .NE. ZERO) THEN
              IF (SPR_ORIENT_1 * SPR_DISP_X + SPR_ORIENT_2 * SPR_DISP_Y .LT. ZERO) THEN
                  SPR_SGN = -ONE
              END IF
          END IF  
C         * Spring deformation is assinged with positive or negative sign
          SPR_DISP = SPR_SGN * SPR_DISP
      ELSE 
          SPR_SGN = ONE
      END IF
!C
C
C     **************** Get spring stiffness and force from nonlinear model ****************
C     * Apply to radial spring, coupled two-spring, modified radial spring *
      IF (KPNT .EQ. 1 .OR. KPNT. EQ. 2 .OR. KPNT .EQ. 5) THEN
C         Use tangent stiffness definition
          I_SPR_NUM = 1
          CALL PINCHING4(PROPS,SVARS,SPR_DISP,SPR_K,SPR_F,KINC,I_SPR_NUM)
C
C         Use secant stiffness definition
          IF (KSEC .EQ. 1 .AND. DABS(SPR_DISP) .GE. TOL) THEN
              SPR_K_SEC = SPR_F / SPR_DISP
              SPR_K = SPR_K_SEC
          END IF
C     * Apply to oriented spring pair *
      ELSE IF (KPNT .EQ. 4) THEN
C
          IF (KINC .EQ. 0 .OR. KINC .EQ. 1) THEN
              SPR_DISP_U = SPR_DISP_X
              SPR_DISP_V = SPR_DISP_Y
          END IF
C           
          I_SPR_NUM = 1
          CALL PINCHING4(PROPS,SVARS,SPR_DISP_U,SPR_K_U,SPR_F_U,KINC,I_SPR_NUM)
C
          I_SPR_NUM = 2
          CALL PINCHING4(PROPS,SVARS,SPR_DISP_V,SPR_K_V,SPR_F_V,KINC,I_SPR_NUM)
C
C         Use secant stiffness definition
          IF (KSEC .EQ. 1. AND. DABS(SPR_DISP_U) .GE. TOL .AND. DABS(SPR_DISP_V) .GE. TOL) THEN
              SPR_K_SEC_U = SPR_F_U / SPR_DISP_U
              SPR_K_SEC_V = SPR_F_V / SPR_DISP_V
              SPR_K_U = SPR_K_SEC_U
              SPR_K_V = SPR_K_SEC_V
          END IF
C
          IF (KINC .EQ. 0 .OR. KINC .EQ. 1) THEN
              SPR_K_X = SPR_K_U
              SPR_F_X = SPR_F_U
              SPR_K_Y = SPR_K_V
              SPR_F_Y = SPR_F_V
          END IF
C
C     * Apply two-spring model
      ELSE 
C         Use tangent stiffness definition
          I_SPR_NUM = 1
          CALL PINCHING4(PROPS,SVARS,SPR_DISP_X,SPR_K_X,SPR_F_X,KINC,I_SPR_NUM)
C
          I_SPR_NUM = 2
          CALL PINCHING4(PROPS,SVARS,SPR_DISP_Y,SPR_K_Y,SPR_F_Y,KINC,I_SPR_NUM)
C
C         Use secant stiffness definition
          IF (KSEC .EQ. 1. AND. DABS(SPR_DISP_X) .GE. TOL .AND. DABS(SPR_DISP_Y) .GE. TOL) THEN
              SPR_K_SEC_X = SPR_F_X / SPR_DISP_X
              SPR_K_SEC_Y = SPR_F_Y / SPR_DISP_Y
              SPR_K_X = SPR_K_SEC_X
              SPR_K_Y = SPR_K_SEC_Y
          END IF
      END IF
C
C
C     **************** Generate stiffness matrix and residual force vector ****************
C     * Radial spring model
      IF (KPNT .EQ. 1 .OR. KPNT .EQ. 5) THEN
          CALL SAMATRX(SPR_AMATRX, SPR_K, SPR_F, SPR_LEN, SPR_COS_X, SPR_COS_Y, SPR_SGN)
          CALL SNFORCE(SPR_F, SPR_COS_X, SPR_COS_Y, SPR_SRESID, SPR_SGN)
C     * Coupled spring model
      ELSE IF (KPNT .EQ. 2) THEN
          CALL SAMATRX_CUP(SPR_AMATRX, SPR_K)
          CALL SNFORCE(SPR_F, SPR_COS_X, SPR_COS_Y, SPR_SRESID, SPR_SGN)
C     * Oriented spring pair model
      ELSE IF (KPNT .EQ. 4) THEN
          IF (KINC. EQ. 1 .OR. KINC .EQ. 0) THEN
              CALL SAMATRX_2(SPR_AMATRX, SPR_K_X, SPR_K_Y) 
              CALL SFORCE_2(SPR_SRESID, SPR_F_X, SPR_F_Y)    
          ELSE 
              CALL SAMATRX_ORNT(SPR_AMATRX, SPR_K_U, SPR_K_V, SVARS)
              CALL SNFORCE_ORNT(SPR_F_U, SPR_F_V, SVARS, SPR_SRESID, SPR_F_X, SPR_F_Y)
          END IF
C     * Uncoupled 2-spring model
      ELSE 
          CALL SAMATRX_2(SPR_AMATRX, SPR_K_X, SPR_K_Y) 
          CALL SFORCE_2(SPR_SRESID, SPR_F_X, SPR_F_Y)
      END IF
C
C
C     **************** Update energy ****************
      IF (KPNT .EQ. 1 .OR. KPNT .EQ. 2 .OR. KPNT .EQ. 5) THEN
          SVARS(141) = SVARS(142)         ! Deformation for last increment
          SVARS(142) = SPR_DISP           ! Deformation at current increment
          SVARS(143) = SVARS(144)         ! Spring force from last increment
          SVARS(144) = SPR_F              ! Spring force at current increment
C
          SPR_DISP1 = SVARS(141)
          SPR_DISP2 = SVARS(142)
          SPR_F1 = SVARS(143)
          SPR_F2 = SVARS(144)
          SPR_ENERGY = HALF * (SPR_F2 + SPR_F1) * (SPR_DISP2 - SPR_DISP1)
          SVARS(145) = SVARS(145) + SPR_ENERGY
      ELSE 
          SVARS(141) = SVARS(142)
          SVARS(142) = SPR_DISP_X
          SVARS(143) = SVARS(144)
          SVARS(144) = SPR_DISP_Y
          SVARS(146) = SVARS(147)
          SVARS(147) = SPR_F_X
          SVARS(148) = SVARS(149)
          SVARS(149) = SPR_F_Y
C
          SPR_DISP_X1 = SVARS(141)
          SPR_DISP_X2 = SVARS(142)
          SPR_DISP_Y1 = SVARS(143)
          SPR_DISP_Y2 = SVARS(144)
          SPR_F_X1 = SVARS(146)
          SPR_F_X2 = SVARS(147)
          SPR_F_Y1 = SVARS(148)
          SPR_F_Y2 = SVARS(149)
          SPR_ENERGY_X = HALF * (SPR_F_X2 + SPR_F_X1) * (SPR_DISP_X2 - SPR_DISP_X1)
          SPR_ENERGY_Y = HALF * (SPR_F_Y2 + SPR_F_Y1) * (SPR_DISP_Y2 - SPR_DISP_Y1)
          SPR_ENERGY = SPR_ENERGY_X + SPR_ENERGY_Y
          SVARS(145) = SVARS(145) + SPR_ENERGY         
      END IF
C
C
C     **************** ABAQUS/Standar analysis procedures ****************
      IF (LFLAGS(3) .EQ. 1) THEN
C         * General static analysis
          IF (LFLAGS(1) .EQ.1 .OR. LFLAGS(1) .EQ. 2) THEN
              DO K1 = 1, 4
                  DO K2 = 1,4
                      AMATRX(K1,K2) = SPR_AMATRX(K1,K2)
                  END DO
                  SRESID(K1) = SPR_SRESID(K1)
                  RHS(K1,1) = RHS(K1,1) - SRESID(K1)
                  ENERGY(2) = SVARS(145)
              END DO
C         * Dynamic analysis (implicit)
          ELSE IF (LFLAGS(1).EQ.11 .OR. LFLAGS(1).EQ.12) THEN
              ALPHA = PARAMS(1)
              BETA  = PARAMS(2)
              GAMMA = PARAMS(3)
              DADU = ONE/(BETA*DTIME**2)
              DVDU = GAMMA/(BETA*DTIME)
C
              DO K1 = 1, NDOFEL
                  AMATRX(K1,K1) = AM*DADU
                  RHS(K1,1) = RHS(K1,1)-AM*A(K1)
              END DO
C
              DO K1 = 1, NDOFEL
                  DO K2 = 1, NDOFEL
                      AMATRX(K1,K2) = AMATRX(K1,K2) + SPR_AMATRX(K1,K2)*(ONE+ALPHA)
                  END DO
                  SRESID(K1) = SPR_SRESID(K1)
              END DO
C
              DO K1 = 1, NDOFEL
                  RHS(K1,1) = RHS(K1,1) - ((ONE+ALPHA)*SRESID(K1)-ALPHA*SVARS(150+K1))
              END DO
C
              ENERGY(1) = ZERO
              DO K1 = 1, NDOFEL
                  SVARS(K1+154) = SVARS(K1+150)   
                  SVARS(K1+150) = SRESID(K1)
                  ENERGY(1) = ENERGY(1)+HALF*V(K1)*AM*V(K1)
              END DO
C
              ENERGY(2) = SVARS(145)
          END IF
C     * Define stiffness matrix only
      ELSE IF (LFLAGS(3) .EQ. 2) THEN
          DO K1 = 1, 4
              DO K2 = 1,4
                  AMATRX(K1,K2) = SPR_AMATRX(K1,K2)
              END DO
          END DO
C     * Define mass matrix
      ELSE IF (LFLAGS(3) .EQ. 4) THEN
          DO K1 = 1, NDOFEL
              DO K2 = 1, NDOFEL
                  AMATRX(K1,K2) = ZERO
              END DO
          END DO
          DO K1 = 1, NDOFEL
              AMATRX(K1,K1) = AM
          END DO
C     * Half-step residual calculation
      ELSE IF (LFLAGS(3) .EQ. 5) THEN
          ALPHA = PARAMS(1)
          DO K1 = 1, NDOFEL
              SRESID(K1) = SPR_SRESID(K1)
          END DO
          DO K1 = 1, NDOFEL
              RHS(K1,1) = RHS(K1,1)-AM*A(K1)-(ONE+ALPHA)*SRESID(K1) + 
     + HALF*ALPHA*(SVARS(K1+150)+SVARS(K1+154))
          END DO
C     * Initial acceleration calculation
      ELSE IF (LFLAGS(3) .EQ. 6) THEN
          DO K1 = 1, NDOFEL
              AMATRX(K1,K1) = AM
              SRESID(K1) = SPR_SRESID(K1)
          END DO
          DO K1 = 1, NDOFEL
              RHS(K1,1) = RHS(K1,1)-SRESID(K1)
          END DO
          ENERGY(1) = ZERO
          DO K1 = 1, NDOFEL
              SVARS(K1+150) = SRESID(K1)
              ENERGY(1) = ENERGY(1)+HALF*V(K1)*AM*V(K1)
          END DO
          ENERGY(2) = SVARS(145)
      END IF
C
C
C     **************** Ouput spring data ****************
C     * Uncoupled two-spring model
800   FORMAT(I10, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8)
C
C     * Radial spring/coupled spring model
900   FORMAT(I10, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8)
C
C     * Oriented spring model
700   FORMAT(I10, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8)
C     
C     * Modified radial spring
950   FORMAT(I10, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8, F20.8,
     +F20.8, F20.8, F20.8)
C
C
      IF (KOUTPUT .EQ. 1) THEN
          IF (KINC .GE. 1) THEN
              WRITE(FILENAME, fmt='(a, I0, a)') FILEPATH, JELEM, ".txt"
              OPEN(300, FILE=FILENAME, STATUS='UNKNOWN', POSITION='APPEND')
C             * Radial spring/coupled two-spring model
              IF (KPNT .EQ. 1 .OR. KPNT .EQ. 2) THEN
                  WRITE(300, 900) KINC, TIME(2), SPR_DISP, SPR_F, SPR_K, ENERGY(2), 
     +SPR_SGN, SPR_COS_X, SPR_COS_Y, SPR_DISP_X, SPR_DISP_Y
C             * Oriented spring pair model
              ELSE IF (KPNT .EQ. 4) THEN
                  WRITE(300, 700) KINC, TIME(2), SPR_DISP_U, SPR_K_U, SPR_F_U, SPR_DISP_V,
     +SPR_K_V, SPR_F_V, SVARS(171), SVARS(172), SPR_DISP_X, SPR_DISP_Y
C             * Modified radial spring
              ELSE IF (KPNT .EQ. 5) THEN
                  WRITE(300, 950) KINC, TIME(2), SPR_DISP, SPR_F, SPR_K, ENERGY(2), 
     +SPR_SGN, SPR_COS_X, SPR_COS_Y, SPR_DISP_X, SPR_DISP_Y, SVARS(180), SVARS(181), SVARS(182)                  
C             * Uncoupled two-spring model
              ELSE 
                  WRITE(300, 800) KINC, TIME(2), SPR_DISP_X, SPR_K_X, SPR_F_X, SPR_DISP_Y, SPR_K_Y, SPR_F_Y
              END IF
              CLOSE(300)
          END IF
      END IF
C
C
      RETURN
      END
C
C
      SUBROUTINE SGEOM(U,COORDS,SPR_LEN,SPR_DISP,SPR_COS_X,SPR_COS_Y,SPR_DISP_X,SPR_DISP_Y,PROPS,SVARS)
C
C     
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION U(4), COORDS(3,2), PROPS(41), SVARS(200)
C
C
      DOUBLE PRECISION SPR_COORD_X, SPR_COORD_Y
      DOUBLE PRECISION SPR_DISP_X, SPR_DISP_Y
      DOUBLE PRECISION SPR_LEN_X, SPR_LEN_Y
      DOUBLE PRECISION SPR_LEN0, SPR_LEN
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
C
      INTEGER ISPR_DIR_1, ISPR_DIR_2
C
C
      ISPR_DIR_1 = INT(PROPS(40))
      ISPR_DIR_2 = INT(PROPS(41))
C
      SPR_COORD_X = COORDS(ISPR_DIR_1,2) - COORDS(ISPR_DIR_1,1)
      SPR_COORD_Y = COORDS(ISPR_DIR_2,2) - COORDS(ISPR_DIR_2,1)
C
      SPR_LEN0 = DSQRT(SPR_COORD_X*SPR_COORD_X+SPR_COORD_Y*SPR_COORD_Y)
C
      SPR_DISP_X = U(3) - U(1)
      SPR_DISP_Y = U(4) - U(2)
C
C     Adjust spring deformation and orientation for 'zero" deformation case
      IF (DABS(SPR_DISP_X) .LE. TOL .OR. DABS(SPR_DISP_Y) .LE. TOL) THEN
          SPR_DISP_X = SPR_DISP_X + TOL * 1.D0/DSQRT(2.D0)
          SPR_DISP_Y = SPR_DISP_Y + TOL * 1.D0/DSQRT(2.D0)
      END IF
C
      SPR_LEN_X = SPR_COORD_X + SPR_DISP_X
      SPR_LEN_Y = SPR_COORD_Y + SPR_DISP_Y
C    
C     * Radial spring length should not be zero!
      SPR_LEN = DSQRT(SPR_LEN_X*SPR_LEN_X+SPR_LEN_Y*SPR_LEN_Y)
C
      SPR_DISP = SPR_LEN - SPR_LEN0
C
      SPR_COS_X = SPR_LEN_X/SPR_LEN
      SPR_COS_Y = SPR_LEN_Y/SPR_LEN
C
C     Record spring orientation 
C     * Discard if unrealistically huge, zero or one
      IF (DABS(SPR_COS_X * SPR_COS_Y) .LT. ONE) THEN
          IF (DABS(SPR_COS_X * SPR_COS_Y) .GT. ZERO) THEN
              SVARS(124) = SPR_COS_X
              SVARS(125) = SPR_COS_Y
          END IF
      END IF
C
C
      RETURN
      END
C
C
      SUBROUTINE SGEOM_MDRD(U,COORDS,SPR_LEN,SPR_DISP,SPR_COS_X,SPR_COS_Y,SPR_DISP_X,SPR_DISP_Y,PROPS,SVARS)
C
C     
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION U(4), COORDS(3,2), PROPS(41), SVARS(200)
C
C
      DOUBLE PRECISION SPR_COORD_X, SPR_COORD_Y
      DOUBLE PRECISION SPR_DISP_X, SPR_DISP_Y
      DOUBLE PRECISION SPR_LEN_X, SPR_LEN_Y
      DOUBLE PRECISION SPR_LEN0, SPR_LEN
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
C
      INTEGER ISPR_DIR_1, ISPR_DIR_2
C
C
      ISPR_DIR_1 = INT(PROPS(40))
      ISPR_DIR_2 = INT(PROPS(41))
C
      SPR_COORD_X = COORDS(ISPR_DIR_1,2) - COORDS(ISPR_DIR_1,1)
      SPR_COORD_Y = COORDS(ISPR_DIR_2,2) - COORDS(ISPR_DIR_2,1)
C
      IF (SVARS(180) .EQ. ONE) THEN
          SPR_ORIENT_1 = SVARS(181)
          SPR_ORIENT_2 = SVARS(182)
      END IF
C
      SPR_COORD_X = SPR_ORIENT_1 * 1.D1
      SPR_COORD_Y = SPR_ORIENT_2 * 1.D1
C
      SPR_LEN0 = DSQRT(SPR_COORD_X*SPR_COORD_X+SPR_COORD_Y*SPR_COORD_Y)
C
      SPR_DISP_X = U(3) - U(1)
      SPR_DISP_Y = U(4) - U(2)
C
C     Adjust spring deformation and orientation for 'zero" deformation case
      IF (DABS(SPR_DISP_X) .LE. TOL .OR. DABS(SPR_DISP_Y) .LE. TOL) THEN
          SPR_DISP_X = SPR_DISP_X + TOL * 1.D0/DSQRT(2.D0)
          SPR_DISP_Y = SPR_DISP_Y + TOL * 1.D0/DSQRT(2.D0)
      END IF
C
      SPR_LEN_X = SPR_COORD_X + SPR_DISP_X
      SPR_LEN_Y = SPR_COORD_Y + SPR_DISP_Y
C    
C     * Radial spring length should not be zero!
      SPR_LEN = DSQRT(SPR_LEN_X*SPR_LEN_X+SPR_LEN_Y*SPR_LEN_Y)
C
      SPR_DISP = SPR_LEN - SPR_LEN0
C
      SPR_COS_X = SPR_LEN_X/SPR_LEN
      SPR_COS_Y = SPR_LEN_Y/SPR_LEN
C
C     Record spring orientation 
C     * Discard if unrealistically huge, zero or one
      IF (DABS(SPR_COS_X * SPR_COS_Y) .LT. ONE) THEN
          IF (DABS(SPR_COS_X * SPR_COS_Y) .GT. ZERO) THEN
              SVARS(124) = SPR_COS_X
              SVARS(125) = SPR_COS_Y
          END IF
      END IF
C
C
      RETURN
      END
C
C
      SUBROUTINE SGEOM_CUP(U,SPR_DISP,SPR_COS_X,SPR_COS_Y,SPR_DISP_X,SPR_DISP_Y)
C
C     
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION U(4)
C
C
      DOUBLE PRECISION SPR_DISP_X, SPR_DISP_Y
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
      DOUBLE PRECISION SPR_DISP
C
C
      SPR_DISP_X = U(3) - U(1)
      SPR_DISP_Y = U(4) - U(2)
C
      SPR_DISP = DSQRT(SPR_DISP_X*SPR_DISP_X + SPR_DISP_Y*SPR_DISP_Y)
C
      SPR_COS_X = SPR_DISP_X/SPR_DISP
      SPR_COS_Y = SPR_DISP_Y/SPR_DISP
C
C
      RETURN
      END
C
C
      SUBROUTINE SGEOM_ORN(U, SPR_DISP_X, SPR_DISP_Y, SPR_COS_X, SPR_COS_Y, SPR_DISP_U, SPR_DISP_V, SVARS)
C
C     
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION U(4), SVARS(200)
C
C
      DOUBLE PRECISION SPR_DISP_X, SPR_DISP_Y
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
      DOUBLE PRECISION SPR_DISP_U, SPR_DISP_V
      DOUBLE PRECISION SPR_DISP
C
C
      SPR_DISP_X = U(3) - U(1)
      SPR_DISP_Y = U(4) - U(2)
      SPR_DISP = DSQRT(SPR_DISP_X*SPR_DISP_X + SPR_DISP_Y*SPR_DISP_Y)
C
C     Adjust spring deformation and orientation for 'zero" deformation case
      IF (DABS(SPR_DISP_X) .LE. TOL .OR. DABS(SPR_DISP_Y) .LE. TOL) THEN
          SPR_COS_X = 1.D0/DSQRT(2.D0)
          SPR_COS_Y = 1.D0/DSQRT(2.D0)
      ELSE 
          SPR_COS_X = SPR_DISP_X/SPR_DISP
          SPR_COS_Y = SPR_DISP_Y/SPR_DISP          
      END IF
C
C     Save spring orientation
      IF (SVARS(173) .NE. ONE) THEN
          IF (SPR_DISP_X .NE. 1.D0/DSQRT(2.D0) .AND. SPR_DISP_X 
     +          .NE. 1.D0/DSQRT(2.D0)) THEN
              SVARS(171) = SPR_COS_X
              SVARS(172) = SPR_COS_Y
              SVARS(173) = ONE
          END IF
      END IF
      
C     Retrieve spring orientation
      IF (SVARS(173) .EQ. ONE) THEN
          SPR_COS_X = SVARS(171)
          SPR_COS_Y = SVARS(172)
      END IF
C
      SPR_DISP_U = SPR_DISP_X * SPR_COS_X + SPR_DISP_Y * SPR_COS_Y
      SPR_DISP_V = -SPR_DISP_X * SPR_COS_Y + SPR_DISP_Y * SPR_COS_X
C
C
      RETURN
      END
C
C
      SUBROUTINE  SAMATRX_2(AMATRX, SPR_K_X, SPR_K_Y) 
C
C    
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION AMATRX(4,4)
C
      DOUBLE PRECISION SPR_K_X, SPR_K_Y
C
C
C     STIFFNESS MATRIX
      AMATRX(1,1) = SPR_K_X
      AMATRX(3,3) = SPR_K_X
      AMATRX(1,3) = -SPR_K_X
      AMATRX(3,1) = -SPR_K_X
      AMATRX(2,2) = SPR_K_Y
      AMATRX(4,4) = SPR_K_Y
      AMATRX(2,4) = -SPR_K_Y
      AMATRX(4,2) = -SPR_K_Y
C
C
      RETURN
      END
C
C
      SUBROUTINE SFORCE_2(SRESID, SPR_F_X, SPR_F_Y)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION SRESID(4)
C
      DOUBLE PRECISION SPR_F_X, SPR_F_Y
C
C
      SRESID(1) = -SPR_F_X
      SRESID(2) = -SPR_F_Y
      SRESID(3) = SPR_F_X
      SRESID(4) = SPR_F_Y
C
C
      RETURN
      END
C
C
      SUBROUTINE  SAMATRX_CUP(AMATRX, SPR_K)
C
C
C    
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION AMATRX(4,4)
C
      DOUBLE PRECISION SPR_K
C
C
      AMATRX(1,1) = SPR_K
      AMATRX(3,3) = SPR_K
      AMATRX(1,3) = -SPR_K
      AMATRX(3,1) = -SPR_K
      AMATRX(2,2) = SPR_K
      AMATRX(4,4) = SPR_K
      AMATRX(4,2) = -SPR_K
      AMATRX(2,4) = -SPR_K
C
C
      RETURN
      END
C
C
      SUBROUTINE  SAMATRX(AMATRX, SPR_K, SPR_F, SPR_LEN, SPR_COS_X, SPR_COS_Y, SPR_SGN)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION AMATRX(4,4)
C
C
      DOUBLE PRECISION SPR_K, SPR_F
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
      DOUBLE PRECISION SPR_LEN
C
      DOUBLE PRECISION SPR_SGN
C     
C     ELASTIC STIFFNESS MATRIX
C
       AMATRX(1,1) = SPR_K * (SPR_COS_X * SPR_COS_X)
       AMATRX(1,2) = SPR_K * (SPR_COS_X * SPR_COS_Y)
       AMATRX(1,3) = SPR_K * (-SPR_COS_X * SPR_COS_X)
       AMATRX(1,4) = SPR_K * (-SPR_COS_X * SPR_COS_Y)
C
       AMATRX(2,1) = SPR_K * (SPR_COS_X * SPR_COS_Y)
       AMATRX(2,2) = SPR_K * (SPR_COS_Y * SPR_COS_Y)
       AMATRX(2,3) = SPR_K * (-SPR_COS_X * SPR_COS_Y)
       AMATRX(2,4) = SPR_K * (-SPR_COS_Y * SPR_COS_Y)
C
       AMATRX(3,1) = SPR_K * (-SPR_COS_X * SPR_COS_X)
       AMATRX(3,2) = SPR_K * (-SPR_COS_X * SPR_COS_Y)
       AMATRX(3,3) = SPR_K * (SPR_COS_X * SPR_COS_X)
       AMATRX(3,4) = SPR_K * (SPR_COS_X * SPR_COS_Y)
C
       AMATRX(4,1) = SPR_K * (-SPR_COS_X * SPR_COS_Y)
       AMATRX(4,2) = SPR_K * (-SPR_COS_Y * SPR_COS_Y)
       AMATRX(4,3) = SPR_K * (SPR_COS_X * SPR_COS_Y)
       AMATRX(4,4) = SPR_K * (SPR_COS_Y * SPR_COS_Y)
C

C
C     ADD GEOMETRIC STIFFNESS MATRIX COMPONENT
      AMATRX(1,1) = AMATRX(1,1) + SPR_F / SPR_LEN * SPR_SGN
      AMATRX(1,3) = AMATRX(1,3) - SPR_F / SPR_LEN * SPR_SGN
      AMATRX(2,2) = AMATRX(2,2) + SPR_F / SPR_LEN * SPR_SGN
      AMATRX(2,4) = AMATRX(2,4) - SPR_F / SPR_LEN * SPR_SGN
      AMATRX(3,1) = AMATRX(3,1) - SPR_F / SPR_LEN * SPR_SGN
      AMATRX(3,3) = AMATRX(3,3) + SPR_F / SPR_LEN * SPR_SGN
      AMATRX(4,2) = AMATRX(4,2) - SPR_F / SPR_LEN * SPR_SGN
      AMATRX(4,4) = AMATRX(4,4) + SPR_F / SPR_LEN * SPR_SGN
C
C
      RETURN
      END
C
C
      SUBROUTINE SNFORCE(SPR_F, SPR_COS_X, SPR_COS_Y, SRESID, SPR_SGN)
C     
C     
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
C
      DIMENSION SRESID(4)
C
      DOUBLE PRECISION SPR_F
      DOUBLE PRECISION SPR_F_X, SPR_F_Y
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
C
      DOUBLE PRECISION SPR_SGN
C
C     NODAL FORCE COMPONENTS
      SPR_F_X = SPR_F * SPR_COS_X * SPR_SGN
      SPR_F_Y = SPR_F * SPR_COS_Y * SPR_SGN
C     
C     UPDATE NODAL FORCE VECTOR
      SRESID(1) = -SPR_F_X
      SRESID(2) = -SPR_F_Y      
      SRESID(3) = SPR_F_X
      SRESID(4) = SPR_F_Y      
C
C
      RETURN
      END
C
C
      SUBROUTINE  SAMATRX_ORNT(AMATRX, SPR_K_U, SPR_K_V, SVARS)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION AMATRX(4,4), SVARS(200)
C
C
      DOUBLE PRECISION SPR_K_U, SPR_K_V
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
C
C
      DOUBLE PRECISION K11, K12, K22
C     
C
      SPR_COS_X = SVARS(171)
      SPR_COS_Y = SVARS(172)
C     
      K11 = SPR_K_U * SPR_COS_X * SPR_COS_X + SPR_K_V * SPR_COS_Y * SPR_COS_Y
      K12 = SPR_K_U * SPR_COS_X * SPR_COS_Y - SPR_K_V * SPR_COS_X * SPR_COS_Y
      K22 = SPR_K_U * SPR_COS_Y * SPR_COS_Y + SPR_K_V * SPR_COS_X * SPR_COS_X
C
      AMATRX(1,1) = K11
      AMATRX(1,2) = K12
      AMATRX(1,3) = -K11
      AMATRX(1,4) = -K12
      AMATRX(2,1) = K12
      AMATRX(2,2) = K22
      AMATRX(2,3) = -K12
      AMATRX(2,4) = -K22
      AMATRX(3,1) = -K11
      AMATRX(3,2) = -K12
      AMATRX(3,3) = K11
      AMATRX(3,4) = K12
      AMATRX(4,1) = -K12
      AMATRX(4,2) = -K22
      AMATRX(4,3) = K12
      AMATRX(4,4) = K22
C
C
      RETURN
      END
C
C
      SUBROUTINE SNFORCE_ORNT(SPR_F_U, SPR_F_V, SVARS, SRESID, SPR_F_X, SPR_F_Y)
C     
C     
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION SRESID(4), SVARS(200)
C
      DOUBLE PRECISION SPR_F_X, SPR_F_Y
      DOUBLE PRECISION SPR_F_U, SPR_F_V
      DOUBLE PRECISION SPR_COS_X, SPR_COS_Y
C
C
      SPR_COS_X = SVARS(171)
      SPR_COS_Y = SVARS(172)
C
C     NODAL FORCE COMPONENTS
      SPR_F_X = SPR_F_U * SPR_COS_X - SPR_F_V * SPR_COS_Y
      SPR_F_Y = SPR_F_U * SPR_COS_Y + SPR_F_V * SPR_COS_X
C     
C     UPDATE NODAL FORCE VECTOR
      SRESID(1) = -SPR_F_X
      SRESID(2) = -SPR_F_Y      
      SRESID(3) = SPR_F_X
      SRESID(4) = SPR_F_Y      
C
C
      RETURN
      END
C
C
C     *****************************************************************
C                             PINCHING4 ROUTINE
C     *****************************************************************
      SUBROUTINE PINCHING4(PROPS,SVARS,SPR_DISP,SPR_K,SPR_F,KINC,I_SPR_NUM)
C
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION PROPS(41), SVARS(200), 
     +            envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6),
     +            envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state3Strain(4), state3Stress(4),
     +            state4Strain(4), state4Stress(4)
C
      INTEGER I_Cstate, I_Tstate
      INTEGER I_DmgCyc
      DOUBLE PRECISION strain, dstrain
      DOUBLE PRECISION Cstrain, Cstress, CstrainRate
      DOUBLE PRECISION Tstrain, Tstress, TstrainRate
      DOUBLE PRECISION P_lowI_CstateStrain, P_lowI_CstateStress
      DOUBLE PRECISION hghI_CstateStrain, hghI_CstateStress
      DOUBLE PRECISION CminStrainDmnd, CmaxStrainDmnd
      DOUBLE PRECISION TminStrainDmnd,TmaxStrainDmnd
      DOUBLE PRECISION elasticStrainEnergy, energyCapacity
      DOUBLE PRECISION Cenergy, CnCycle
      DOUBLE PRECISION Tenergy, TnCycle
      DOUBLE PRECISION CgammaK, CgammaD, CgammaF
      DOUBLE PRECISION TgammaD, TgammaK, TgammaF
      DOUBLE PRECISION gammaKUsed, gammaFUsed
      DOUBLE PRECISION Ttangent, P_kunload
      DOUBLE PRECISION P_kElasticPosDamgd, P_kElasticNegDamgd
      DOUBLE PRECISION P_kElasticPos,P_kElasticNeg
      DOUBLE PRECISION uMaxDamgd, uMinDamgd
C
      DOUBLE PRECISION SPR_K, SPR_F, SPR_DISP
C
      INTEGER I_SPR_NUM
      
C
C     ASSIGN SVARS VALUES TO PINCHING4 LOCAL VARIABLES
      CALL SUEL2PIN(envlpPosDamgdStress, envlpNegDamgdStress,
     +state3Strain, state3Stress, state4Strain, state4Stress,
     +I_Cstate, Cstrain, Cstress, CstrainRate, P_lowI_CstateStrain,
     +P_lowI_CstateStress, hghI_CstateStrain, hghI_CstateStress, 
     +CminStrainDmnd, CmaxStrainDmnd, Cenergy, CgammaK,
     +CgammaD, CgammaF, Ttangent, gammaKUsed, gammaFUsed,
     +P_kElasticPosDamgd, P_kElasticNegDamgd, uMaxDamgd,uMinDamgd,
     +P_kunload, CnCycle, elasticStrainEnergy, 
     +SPR_F, SPR_K, SVARS, I_SPR_NUM)
C
C     CONVERT UEL VARIABLES TO PINCHING4 LOCAL VARIABLES
      strain = SPR_DISP
C
C     *****************************************************************
      CALL SetEnvelop(PROPS, envlpPosStrain,
     +            envlpPosStress, envlpNegStrain, envlpNegStress, 
     +            P_kElasticPos,P_kElasticNeg,energyCapacity)
C
C    
      IF (KINC .EQ. 1 .OR. KINC .EQ. 0) THEN
          CALL revertToStart(envlpPosStrain, envlpPosStress,
     +            envlpNegStrain, envlpNegStress,
     +            P_kElasticPos,P_kElasticNeg,
     +            I_Cstate,Cstrain,Cstress,CstrainRate,
     +            P_lowI_CstateStrain,P_lowI_CstateStress,
     +            hghI_CstateStrain,hghI_CstateStress,
     +            CminStrainDmnd,CmaxStrainDmnd,
     +            Cenergy,CgammaK,CgammaD,CgammaF,CnCycle,
     +            Ttangent,dstrain,gammaKUsed,gammaFUsed,
     +            P_kElasticPosDamgd,P_kElasticNegDamgd,
     +            uMaxDamgd,uMinDamgd,
     +            envlpPosDamgdStress, envlpNegDamgdStress)
C
C
      ELSE 
          CALL revertToLastCommit(I_Cstate,CstrainRate,
     +            P_lowI_CstateStrain,P_lowI_CstateStress, 
     +            hghI_CstateStrain,hghI_CstateStress,
     +            CminStrainDmnd,CmaxStrainDmnd, 
     +            Cenergy,Cstrain,Cstress,
     +            CgammaD,CgammaK,CgammaF,CnCycle,
     +            I_Tstate,TstrainRate,
     +            P_lowI_TstateStrain,P_lowI_TstateStress,
     +            hghI_TstateStrain,hghI_TstateStress,
     +            TminStrainDmnd,TmaxStrainDmnd,
     +            Tenergy,Tstrain,Tstress,
     +            TgammaD,TgammaK,TgammaF,TnCycle)
      END IF
C
C
      CALL setTrialStrain(strain,CstrainRate,I_Cstate,Cenergy,
     +                    P_lowI_CstateStrain,hghI_CstateStrain,
     +                    P_lowI_CstateStress,hghI_CstateStress,
     +                    CminStrainDmnd,CmaxStrainDmnd,
     +                    CgammaF,CgammaK,CgammaD,
     +                    envlpPosStress,envlpPosStrain,
     +                    P_kElasticPosDamgd,P_kElasticNegDamgd,
     +                    state3Strain,state3Stress,
     +                    P_kunload,state4Strain,state4Stress,Cstrain,
     +                    uMaxDamgd,uMinDamgd,
     +                    envlpNegStrain,envlpNegStress,
     +                    P_kElasticPos,P_kElasticNeg,Cstress,I_DmgCyc,
     +                    CnCycle,energyCapacity,
     +                    I_Tstate,Tenergy,Tstrain,
     +                    P_lowI_TstateStrain,hghI_TstateStrain,
     +                    P_lowI_TstateStress,hghI_TstateStress,
     +                    TgammaF,TgammaK,TgammaD,
     +                    dstrain,Ttangent,Tstress,elasticStrainEnergy,
     +                    TminStrainDmnd,TmaxStrainDmnd,
     +                    gammaKUsed,gammaFUsed,
     +                    envlpPosDamgdStress,envlpNegDamgdStress,
     +                    TnCycle, PROPS)
C
C
      CALL commitState(I_Tstate,dstrain,TstrainRate,
     +                P_lowI_TstateStrain,P_lowI_TstateStress,
     +                hghI_TstateStrain,hghI_TstateStress,
     +                TminStrainDmnd,TmaxStrainDmnd,
     +                Tenergy,Tstress,Tstrain,
     +                TgammaK,TgammaD,TgammaF,
     +                P_kElasticPos,P_kElasticNeg,
     +                gammaKUsed,gammaFUsed,
     +                envlpPosStress,envlpNegStress,TnCycle,
     +                I_Cstate,CstrainRate,
     +                P_lowI_CstateStrain,P_lowI_CstateStress,
     +                hghI_CstateStrain,hghI_CstateStress,
     +                CminStrainDmnd,CmaxStrainDmnd,
     +                Cenergy,Cstress,Cstrain,
     +                CgammaK,CgammaD,CgammaF,
     +                P_kElasticPosDamgd,P_kElasticNegDamgd,
     +                uMaxDamgd,uMinDamgd,
     +                envlpPosDamgdStress,envlpNegDamgdStress,CnCycle)
C     *****************************************************************
C
C     CONVERT PINCHING4 LOCAL VARIABELS TO UEL VARAIBLES
      SPR_F = Cstress
      SPR_K = Ttangent
C
C     SAVE PINCHING4 LOCAL VARIABLES TO SVARS
      CALL SPIN2UEL(envlpPosDamgdStress, envlpNegDamgdStress,
     +state3Strain, state3Stress, state4Strain, state4Stress,
     +I_Cstate, Cstrain, Cstress, CstrainRate, P_lowI_CstateStrain,
     +P_lowI_CstateStress, hghI_CstateStrain, hghI_CstateStress, 
     +CminStrainDmnd, CmaxStrainDmnd, Cenergy, CgammaK,
     +CgammaD, CgammaF, Ttangent, gammaKUsed, gammaFUsed,
     +P_kElasticPosDamgd, P_kElasticNegDamgd, uMaxDamgd,uMinDamgd,
     +P_kunload, CnCycle, elasticStrainEnergy, strain, dstrain, 
     +SPR_DISP, SPR_F, SPR_K, SVARS, I_SPR_NUM)
C
C
      RETURN
      END
C
C
C     ***************************************
C         PINCHING4 FUNCTION: SetEnvelop()
C     ***************************************
      SUBROUTINE SetEnvelop(PROPS, envlpPosStrain,
     +            envlpPosStress, envlpNegStrain, envlpNegStress, 
     +            P_kElasticPos,P_kElasticNeg,energyCapacity)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION PROPS(41), 
     +            envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6)
C
      DOUBLE PRECISION strain1p, strain2p, strain3p, strain4p
      DOUBLE PRECISION stress1p, stress2p, stress3p, stress4p
      DOUBLE PRECISION strain1n, strain2n, strain3n, strain4n
      DOUBLE PRECISION stress1n, stress2n, stress3n, stress4n
      DOUBLE PRECISION gE
      DOUBLE PRECISION P_kPos, P_kNeg, P_k
      DOUBLE PRECISION u
      DOUBLE PRECISION P_k1, P_k2
      DOUBLE PRECISION P_kElasticPos, P_kElasticNeg
      DOUBLE PRECISION energypos, energyneg
      DOUBLE PRECISION P_max_energy, energyCapacity
C
C     UNZIP PROPS(*) VALUES
      strain1p = PROPS(1)
      strain2p = PROPS(2)
      strain3p = PROPS(3)
      strain4p = PROPS(4)
      stress1p = PROPS(5)
      stress2p = PROPS(6)
      stress3p = PROPS(7)
      stress4p = PROPS(8)
C
      strain1n = PROPS(9)
      strain2n = PROPS(10)
      strain3n = PROPS(11)
      strain4n = PROPS(12)
      stress1n = PROPS(13)
      stress2n = PROPS(14)
      stress3n = PROPS(15)
      stress4n = PROPS(16)

C     **************************************
      gE = PROPS(38)     
C
      P_kPos = stress1p/strain1p
      P_kNeg = stress1n/strain1n
      P_k = MAX(P_kPos, P_kNeg)
C
      IF (strain1p > -ONE*strain1n) THEN
          u = (1.0D-8)*strain1p
      ELSE 
          u = (-1.0D-8)*strain1n
      END IF
C
      envlpPosStrain(1) = u
      envlpPosStress(1) = u*P_k
      envlpNegStrain(1) = -u
      envlpNegStress(1) = -u*P_k
C
      envlpPosStrain(2) = strain1p
      envlpPosStrain(3) = strain2p
      envlpPosStrain(4) = strain3p
      envlpPosStrain(5) = strain4p
C
      envlpNegStrain(2) = strain1n
      envlpNegStrain(3) = strain2n
      envlpNegStrain(4) = strain3n
      envlpNegStrain(5) = strain4n
C
      envlpPosStress(2) = stress1p
      envlpPosStress(3) = stress2p
      envlpPosStress(4) = stress3p
      envlpPosStress(5) = stress4p
C
      envlpNegStress(2) = stress1n
      envlpNegStress(3) = stress2n
      envlpNegStress(4) = stress3n
      envlpNegStress(5) = stress4n
C
      P_k1 = (stress4p - stress3p)/(strain4p - strain3p)
      P_k2 = (stress4n - stress3n)/(strain4n - strain3n)
C
      envlpPosStrain(6) = 1.0D+6*strain4p
      IF (P_k1 .GT. ZERO) THEN
          envlpPosStress(6) = stress4p+P_k1*(envlpPosStrain(6)-strain4p)
      ELSE
          envlpPosStress(6) = stress4p*(ONE+PONE)
      END IF
      envlpNegStrain(6) = 1.0D+6*strain4n
      IF (P_k2 .GT. ZERO) THEN
          envlpNegStress(6) = stress4n+P_k2*(envlpNegStrain(6)-strain4n)
      ELSE
           envlpNegStress(6) = stress4n*(ONE+PONE)
      END IF
C
C     define crtical material properties
      P_kElasticPos = envlpPosStress(2)/envlpPosStrain(2)
      P_kElasticNeg = envlpNegStress(2)/envlpNegStrain(2)
C
      energypos = HALF*envlpPosStrain(1)*envlpPosStress(1)
      DO J = 1, 4
           energypos = energypos+HALF*(envlpPosStress(J) + 
     +        envlpPosStress(J+1))*(envlpPosStrain(J+1) - 
     +        envlpPosStrain(J))
      END DO
C
      energyneg = HALF*envlpNegStrain(1)*envlpNegStress(1)
      DO J = 1, 4
          energyneg = energyneg+HALF*(envlpNegStress(J) + 
     +        envlpNegStress(J+1))*(envlpNegStrain(J+1) - 
     +        envlpNegStrain(J))
      END DO
C
C
      P_max_energy = MAX(energypos, energyneg)
      energyCapacity = gE*P_max_energy
C
C
      RETURN 
      END
C
C
C     ***************************************
C         PINCHING4: revertToStart
C     ***************************************
      SUBROUTINE revertToStart(envlpPosStrain, envlpPosStress,
     +            envlpNegStrain, envlpNegStress,
     +            P_kElasticPos,P_kElasticNeg,
     +            I_Cstate,Cstrain,Cstress,CstrainRate,
     +            P_lowI_CstateStrain,P_lowI_CstateStress,
     +            hghI_CstateStrain,hghI_CstateStress,
     +            CminStrainDmnd,CmaxStrainDmnd,
     +            Cenergy,CgammaK,CgammaD,CgammaF,CnCycle,
     +            Ttangent,dstrain,gammaKUsed,gammaFUsed,
     +            P_kElasticPosDamgd,P_kElasticNegDamgd,
     +            uMaxDamgd,uMinDamgd,
     +            envlpPosDamgdStress, envlpNegDamgdStress)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6),
     +            envlpPosDamgdStress(6), envlpNegDamgdStress(6)
C
      INTEGER I_Cstate
      DOUBLE PRECISION Cstrain, Cstress, CstrainRate
      DOUBLE PRECISION dstrain
      DOUBLE PRECISION P_lowI_CstateStrain, P_lowI_CstateStress
      DOUBLE PRECISION hghI_CstateStrain, hghI_CstateStress
      DOUBLE PRECISION CminStrainDmnd, CmaxStrainDmnd
      DOUBLE PRECISION Cenergy
      DOUBLE PRECISION CnCycle
      DOUBLE PRECISION CgammaK, CgammaD, CgammaF
      DOUBLE PRECISION gammaKUsed, gammaFUsed
      DOUBLE PRECISION Ttangent
      DOUBLE PRECISION P_kElasticPosDamgd, P_kElasticNegDamgd
      DOUBLE PRECISION P_kElasticPos, P_kElasticNeg
      DOUBLE PRECISION uMaxDamgd, uMinDamgd

C
      I_Cstate = 0
      Cstrain = ZERO
      Cstress = ZERO
      CstrainRate = ZERO
      P_lowI_CstateStrain = envlpNegStrain(1)
      P_lowI_CstateStress = envlpNegStress(1)
      hghI_CstateStrain = envlpPosStrain(1)
      hghI_CstateStress = envlpPosStress(1)
      CminStrainDmnd = envlpNegStrain(2)
      CmaxStrainDmnd = envlpPosStrain(2)
      Cenergy = ZERO
      CgammaK = ZERO
      CgammaD = ZERO
      CgammaF = ZERO
      CnCycle = ZERO
C
      Ttangent = envlpPosStress(1)/envlpPosStrain(1)
      dstrain = ZERO
      gammaKUsed = ZERO
      gammaFUsed = ZERO
C
      P_kElasticPosDamgd = P_kElasticPos
      P_kElasticNegDamgd = P_kElasticNeg
      uMaxDamgd = CmaxStrainDmnd
      uMinDamgd = CminStrainDmnd
C
C     INITIALIZE DAMAGED BACKBONE - envlpPosDamgdStress, envlpNegDamgdStress
      DO I = 1, 6
          envlpPosDamgdStress(I) = envlpPosStress(I)
          envlpNegDamgdStress(I) = envlpNegStress(I)
      END DO
C
C
      RETURN
      END
C
C
C     ***************************************
C         PINCHING4: revertToLastCommit
C     ***************************************
      SUBROUTINE revertToLastCommit(I_Cstate,CstrainRate,
     +            P_lowI_CstateStrain,P_lowI_CstateStress, 
     +            hghI_CstateStrain,hghI_CstateStress,
     +            CminStrainDmnd,CmaxStrainDmnd, 
     +            Cenergy,Cstrain,Cstress,
     +            CgammaD,CgammaK,CgammaF,CnCycle,
     +            I_Tstate,TstrainRate,
     +            P_lowI_TstateStrain,P_lowI_TstateStress,
     +            hghI_TstateStrain,hghI_TstateStress,
     +            TminStrainDmnd,TmaxStrainDmnd,
     +            Tenergy,Tstrain,Tstress,
     +            TgammaD,TgammaK,TgammaF,TnCycle)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6)
C
      INTEGER I_Tstate, I_Cstate
      DOUBLE PRECISION TstrainRate
      DOUBLE PRECISION CstrainRate
      DOUBLE PRECISION P_lowI_TstateStrain, P_lowI_TstateStress
      DOUBLE PRECISION hghI_TstateStrain, hghI_TstateStress
      DOUBLE PRECISION P_lowI_CstateStrain, P_lowI_CstateStress
      DOUBLE PRECISION hghI_CstateStrain, hghI_CstateStress
      DOUBLE PRECISION TminStrainDmnd, TmaxStrainDmnd
      DOUBLE PRECISION CminStrainDmnd, CmaxStrainDmnd
      DOUBLE PRECISION Tenergy
      DOUBLE PRECISION Cenergy
      DOUBLE PRECISION Tstrain, Tstress
      DOUBLE PRECISION Cstrain, Cstress
      DOUBLE PRECISION TgammaD, Tgamma, TgammaF
      DOUBLE PRECISION CgammaD, CgammaK, CgammaF
      DOUBLE PRECISION TnCycle
      DOUBLE PRECISION CnCycle
C
      I_Tstate = I_Cstate
C
      TstrainRate = CstrainRate
C
      P_lowI_TstateStrain = P_lowI_CstateStrain
      P_lowI_TstateStress = P_lowI_CstateStress
      hghI_TstateStrain = hghI_CstateStrain
      hghI_TstateStress = hghI_CstateStress
      TminStrainDmnd = CminStrainDmnd
      TmaxStrainDmnd = CmaxStrainDmnd
      Tenergy = Cenergy
C
      Tstrain = Cstrain
      Tstress = Cstress
C
      TgammaD = CgammaD
      TgammaK = CgammaK
      TgammaF = CgammaF
C
      TnCycle = CnCycle
C
C
      RETURN
      END
C
C
C     ***************************************
C           PINCHING4: setTrialStrain
C     ***************************************
      SUBROUTINE setTrialStrain(strain,CstrainRate,I_Cstate,Cenergy,
     +                    P_lowI_CstateStrain,hghI_CstateStrain,
     +                    P_lowI_CstateStress,hghI_CstateStress,
     +                    CminStrainDmnd,CmaxStrainDmnd,
     +                    CgammaF,CgammaK,CgammaD,
     +                    envlpPosStress,envlpPosStrain,
     +                    P_kElasticPosDamgd,P_kElasticNegDamgd,
     +                    state3Strain,state3Stress,
     +                    P_kunload,state4Strain,state4Stress,Cstrain,
     +                    uMaxDamgd,uMinDamgd,
     +                    envlpNegStrain,envlpNegStress,
     +                    P_kElasticPos,P_kElasticNeg,Cstress,I_DmgCyc,
     +                    CnCycle,energyCapacity,
     +                    I_Tstate,Tenergy,Tstrain,
     +                    P_lowI_TstateStrain,hghI_TstateStrain,
     +                    P_lowI_TstateStress,hghI_TstateStress,
     +                    TgammaF,TgammaK,TgammaD,
     +                    dstrain,Ttangent,Tstress,elasticStrainEnergy,
     +                    TminStrainDmnd,TmaxStrainDmnd,
     +                    gammaKUsed,gammaFUsed,
     +                    envlpPosDamgdStress,envlpNegDamgdStress,
     +                    TnCycle, PROPS)
C     
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION PROPS(41), 
     +            envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6),
     +            envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state3Strain(4), state3Stress(4),
     +            state4Strain(4), state4Stress(4)
C
      INTEGER I_Tstate, I_Cstate
      INTEGER I_DmgCyc
      DOUBLE PRECISION Tenergy
      DOUBLE PRECISION Cenergy
      DOUBLE PRECISION strain, dstrain
      DOUBLE PRECISION Cstrain, Cstress, CstrainRate
      DOUBLE PRECISION Tstrain, Tstress
      DOUBLE PRECISION Ttangent, P_kunload
      DOUBLE PRECISION P_lowI_TstateStrain, hghI_TstateStrain
      DOUBLE PRECISION P_lowI_CstateStrain, hghI_CstateStrain
      DOUBLE PRECISION P_lowI_TstateStress, hghI_TstateStress
      DOUBLE PRECISION P_lowI_CstateStress, hghI_CstateStress
      DOUBLE PRECISION TminStrainDmnd, TmaxStrainDmnd
      DOUBLE PRECISION CminStrainDmnd, CmaxStrainDmnd
      DOUBLE PRECISION TgammaF, TgammaK, TgammaD
      DOUBLE PRECISION CgammaF, CgammaK, CgammaD
      DOUBLE PRECISION uMaxDamgd, uMinDamgd
      DOUBLE PRECISION P_kElasticPos, P_kElasticNeg
      DOUBLE PRECISION gammaFUsed, gammaKUsed
      DOUBLE PRECISION P_kElasticPosDamgd, P_kElasticNegDamgd
      DOUBLE PRECISION denergy, elasticStrainEnergy
      DOUBLE PRECISION CnCycle, TnCycle
C
      DOUBLE PRECISION EXTSTRESS
      DOUBLE PRECISION P_k, f
      
C
C     INITIALIZE TRIAL PARAMETERS AS THE LAST CONVERGED ONES
      I_Tstate = I_Cstate
      Tenergy = Cenergy
      Tstrain = strain
      P_lowI_TstateStrain = P_lowI_CstateStrain
      hghI_TstateStrain = hghI_CstateStrain
      P_lowI_TstateStress = P_lowI_CstateStress
      hghI_TstateStress = hghI_CstateStress
      TminStrainDmnd = CminStrainDmnd
      TmaxStrainDmnd = CmaxStrainDmnd
      TgammaF = CgammaF
      TgammaK = CgammaK
      TgammaD = CgammaD
C
C
      dstrain = Tstrain - Cstrain
      IF (dstrain  .LT. 1.0D-12 .AND. dstrain .GT.-1.0D-12) THEN
          dstrain = ZERO
      END IF
C      
C     DETERMINE IF THERE IS A CAHNGE IN STATE
      CALL getstate(Tstrain,dstrain, CstrainRate, Cstrain, Cstress, 
     +                    envlpPosStrain, envlpPosStress,
     +                    envlpNegStrain, envlpNegStress,
     +                    uMaxDamgd, uMinDamgd, CgammaF, CgammaK,
     +                    P_kElasticPos, P_kElasticNeg,  
     +                    P_lowI_TstateStrain, P_lowI_TstateStress,
     +                    hghI_TstateStrain, hghI_TstateStress,
     +                    TmaxStrainDmnd, TminStrainDmnd, 
     +                    gammaFUsed, gammaKUsed, 
     +                    envlpNegDamgdStress, envlpPosDamgdStress, 
     +                    P_kElasticPosDamgd, P_kElasticNegDamgd,I_Tstate)
C
C
C
      IF (I_Tstate .GE. 0) THEN
          IF (I_Tstate .EQ. 0) THEN
              Ttangent = envlpPosStress(1)/envlpPosStrain(1)
              Tstress = Ttangent*Tstrain
          ELSE IF (I_Tstate .EQ. 1) THEN
              CALL posEnvlpTangent(strain,envlpPosDamgdStress,
     +         envlpPosStrain, P_k)
              Ttangent = P_k
              CALL posEnvlpStress(strain,envlpPosDamgdStress,
     +        envlpPosStrain, EXTSTRESS)  
              Tstress = EXTSTRESS
           ELSE IF (I_Tstate .EQ. 2) THEN
              CALL negEnvlpTangent(strain,envlpNegDamgdStress,
     +         envlpNegStrain, P_k)
              Ttangent = P_k
              CALL negEnvlpStress(strain,envlpNegDamgdStress,
     +        envlpNegStrain, EXTSTRESS)  
              Tstress = EXTSTRESS
C             DEFINITELY WRONG HERE
          ELSE IF (I_Tstate .EQ. 3) THEN
              IF (hghI_TstateStrain .LT. ZERO) THEN
                  P_kunload = P_kElasticNegDamgd
              ELSE
                  P_kunload = P_kElasticPosDamgd
              END IF
              state3Strain(1) = P_lowI_TstateStrain
              state3Strain(4) = hghI_TstateStrain
              state3Stress(1) = P_lowI_TstateStress
              state3Stress(4) = hghI_TstateStress
              CALL getstate3(state3Strain, state3Stress, P_kunload,
     +    P_kElasticNegDamgd, P_lowI_TstateStrain, P_lowI_TstateStress, 
     +    hghI_TstateStrain, hghI_TstateStress, TminStrainDmnd, 
     +    envlpNegStrain, envlpNegDamgdStress, PROPS)  
              CALL Envlp3Tangent(state3Strain,state3Stress,strain, P_k)
              Ttangent = P_k
              CALL Envlp3Stress(state3Strain,state3Stress,strain, f)
              Tstress = f
          ELSE IF (I_Tstate .EQ. 4) THEN
              IF (P_lowI_TstateStrain .LT. ZERO) THEN
                  P_kunload = P_kElasticNegDamgd
              ELSE
                  P_kunload = P_kElasticPosDamgd
              END IF
              state4Strain(1) = P_lowI_TstateStrain
              state4Strain(4) = hghI_TstateStrain
              state4Stress(1) = P_lowI_TstateStress
              state4Stress(4) = hghI_TstateStress
              CALL getstate4(state4Strain, state4Stress, P_kunload,
     +          P_kElasticPosDamgd, P_lowI_TstateStrain,P_lowI_TstateStress,
     +          hghI_TstateStrain, hghI_TstateStress, TmaxStrainDmnd, 
     +          envlpPosStrain, envlpPosDamgdStress, PROPS)
              CALL Envlp4Tangent(state4Strain,state4Stress, strain, P_k)
              Ttangent = P_k
              CALL Envlp4Stress(state4Strain,state4Stress, strain, f)
              Tstress = f
          END IF
      END IF
C
C     UPDATE ENERGY DISSIPATION
      denergy = HALF*(Tstress+Cstress)*dstrain
      IF (Tstrain .GT. ZERO) THEN
          elasticStrainEnergy = HALF*Tstress/P_kElasticPosDamgd*Tstress
      ELSE
          elasticStrainEnergy = HALF*Tstress/P_kElasticNegDamgd*Tstress
      END IF
      Tenergy = Cenergy + denergy
C
C     UPDATE DAMAGE
      CALL updateDmg(Tstrain,dstrain, 
     +            TmaxStrainDmnd, TminStrainDmnd,
     +            envlpPosStrain, envlpNegStrain,
     +            envlpPosDamgdStress,envlpNegDamgdStress,
     +            CnCycle,Tenergy,energyCapacity,
     +            I_DmgCyc, elasticStrainEnergy,
     +            P_kElasticPos,P_kElasticNeg,
     +            TnCycle,TgammaK,TgammaD,TgammaF,
     +            PROPS)     
C
C
      RETURN
      END
C
C
C     ***************************************
C              PINCHING4: getstate
C     ***************************************
      SUBROUTINE getstate(SU, SDU, CstrainRate, Cstrain, Cstress, 
     +                    envlpPosStrain, envlpPosStress,
     +                    envlpNegStrain, envlpNegStress,
     +                    uMaxDamgd, uMinDamgd, CgammaF, CgammaK,
     +                    P_kElasticPos, P_kElasticNeg,  
     +                    P_lowI_TstateStrain, P_lowI_TstateStress,
     +                    hghI_TstateStrain, hghI_TstateStress,
     +                    TmaxStrainDmnd, TminStrainDmnd, 
     +                    gammaFUsed, gammaKUsed, 
     +                    envlpNegDamgdStress, envlpPosDamgdStress, 
     +                    P_kElasticPosDamgd, P_kElasticNegDamgd, I_Tstate)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION PROPS(41), 
     +            envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6),
     +            envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state3Strain(4), state3Stress(4),
     +            state4Strain(4), state4Stress(4)
C
      LOGICAL cid, cis
C
      INTEGER I_Cstate, I_Tstate
      INTEGER newState
      DOUBLE PRECISION Cstrain, Cstress, CstrainRate
      DOUBLE PRECISION P_lowI_TstateStrain, hghI_TstateStrain
      DOUBLE PRECISION P_lowI_TstateStress, hghI_TstateStress
      DOUBLE PRECISION TmaxStrainDmnd, TminStrainDmnd
      DOUBLE PRECISION uMaxDamgd, uMinDamgd
      DOUBLE PRECISION CgammaF, CgammaK
      DOUBLE PRECISION gammaFUsed, gammaKUsed
      DOUBLE PRECISION P_kElasticPos, P_kElasticNeg
      DOUBLE PRECISION P_kElasticPosDamgd, P_kElasticNegDamgd
C
      DOUBLE PRECISION SU, SDU
      DOUBLE PRECISION EXTSTRESS
C
C	INITIALIZE LOCAL VARIABLES
	cid = .false.			! CHANGE IN DIRECTION
      cis = .false.           ! CHANGE IN STATE
	newState = 0	        ! NEW SPRING STATE DETERMINED
C
C     INITIALIZE VARIABLES FOR CALLING EXTERNAL SUBROUTINES
      EXTSTRESS = ZERO
C	
    	IF (SDU * CstrainRate .LE. ZERO) THEN
		cid = .true.
    	END IF
C	
      IF (SU .LT. P_lowI_TstateStrain .OR. SU .GT. hghI_TstateStrain .OR.
     +		cid) THEN
          IF (I_Tstate .EQ. 0) THEN
              IF (SU .GT. hghI_TstateStrain) THEN
                  cis = .true.
                  newstate = 1      
                  P_lowI_TstateStrain = envlpPosStrain(1)
                  P_lowI_TstateStress = envlpPosStress(1)
                  hghI_TstateStrain = envlpPosStrain(6)
                  hghI_TstateStress = envlpPosStress(6)
              ELSE IF (SU .LT. P_lowI_TstateStrain) THEN             
                  cis = .true.
                  newstate = 2
                  P_lowI_TstateStrain = envlpNegStrain(6)
                  P_lowI_TstateStress = envlpNegStress(6)
                  hghI_TstateStrain = envlpNegStrain(1)
                  hghI_TstateStress = envlpNegStress(1)
              END IF
          ELSE IF (I_Tstate .EQ. 1 .AND. SDU .LT. ZERO) THEN
              cis = .true. 
              IF (Cstrain .GT. TmaxStrainDmnd) THEN
                  TmaxStrainDmnd = SU - SDU      
              END IF
              IF (TmaxStrainDmnd .LT. uMaxDamgd) THEN
                  TmaxStrainDmnd = uMaxDamgd    
              END IF
              IF (SU .LT. uMinDamgd) THEN
                  newState = 2
                  gammaFUsed = CgammaF    
                  DO I = 1, 6
                      envlpNegDamgdStress(I) = envlpNegStress(I) * 
     +                    (ONE - gammaFUsed)
                  END DO
                  P_lowI_TstateStrain = envlpNegStrain(6)
                  P_lowI_TstateStress = envlpNegStress(6)
                  hghI_TstateStrain = envlpNegStrain(1)
                  hghI_TstateStress = envlpNegStress(1)
              ELSE
                  newstate = 3
                  gammaFUsed = CgammaF            
                  DO I = 1, 6
                      envlpNegDamgdStress(I) = envlpNegStress(I) * 
     +                    (ONE - gammaFUsed)
                  END DO
                  P_lowI_TstateStrain = uMinDamgd
                  CALL negEnvlpStress(uMinDamgd, envlpNegDamgdStress,
     +                                envlpNegStrain, EXTSTRESS)
                  P_lowI_TstateStress = EXTSTRESS
                  hghI_TstateStrain = Cstrain
                  hghI_TstateStress = Cstress
                  gammaKUsed = CgammaK
                  P_kElasticPosDamgd = P_kElasticPos * (ONE-gammaKUsed)
              END IF
          ELSE IF (I_Tstate .EQ. 2 .AND. SDU .GT. ZERO) THEN
              cis = .true.
              IF (Cstrain .LT. TminStrainDmnd) THEN
                  TminStrainDmnd = Cstrain
              END IF
              IF (TminStrainDmnd .GT. uMinDamgd) THEN
                  TminStrainDmnd = uMinDamgd
              END IF
              IF (SU .GT. uMaxDamgd) THEN
                  newState = 1        
                  gammaFUsed = CgammaF                   
                  DO I = 1, 6
                      envlpPosDamgdStress(I) = envlpPosStress(I) * 
     +                    (ONE - gammaFUsed)
                  END DO           
                  P_lowI_TstateStrain = envlpPosStrain(1)
                  P_lowI_TstateStress = envlpPosStress(1)
                  hghI_TstateStrain = envlpPosStrain(6)
                  hghI_TstateStress = envlpPosStress(6)
              ELSE 
                  newState = 4
                  gammaFUsed = CgammaF    
                  DO I = 1, 6
                      envlpPosDamgdStress(I) = envlpPosStress(I) * 
     +                    (ONE - gammaFUsed)
                  END DO
                  P_lowI_TstateStrain = Cstrain
                  P_lowI_TstateStress = Cstress
                  hghI_TstateStrain = uMaxDamgd
                  CALL posEnvlpStress(uMaxDamgd, envlpPosDamgdStress, 
     +                                envlpPosStrain, EXTSTRESS)
                  hghI_TstateStress = EXTSTRESS
                  gammaKUsed = CgammaK;
                  P_kElasticNegDamgd = P_kElasticNeg * (ONE- gammaKUsed)
              END IF
          ELSE IF (I_Tstate .EQ. 3) THEN
              IF (SU .LT. P_lowI_TstateStrain) THEN
                  cis = .true.
                  newState = 2
                  P_lowI_TstateStrain = envlpNegStrain(6)
                  P_lowI_TstateStress = envlpNegDamgdStress(6)
                  hghI_TstateStrain = envlpNegStrain(1)
                  hghI_TstateStress = envlpNegDamgdStress(1)
              ELSE IF (SU .GT. uMaxDamgd .AND. SDU .GT. ZERO) THEN
                  cis = .true.
                  newState = 1
                  P_lowI_TstateStrain = envlpPosStrain(1)
                  P_lowI_TstateStress = envlpPosStress(1)
                  hghI_TstateStrain = envlpPosStrain(6)
                  hghI_TstateStress = envlpPosStress(6)
              ELSE IF (SDU .GT. ZERO) THEN
                  cis = .true.
                  newState = 4
                  gammaFUsed = CgammaF    
                  P_lowI_TstateStrain = Cstrain
                  P_lowI_TstateStress = Cstress
                  hghI_TstateStrain = uMaxDamgd
                  DO I = 1, 6
                      envlpPosDamgdStress(I) = envlpPosStress(I) * 
     +                    (ONE - gammaFUsed)
                  END DO
                  CALL posEnvlpStress(uMaxDamgd, envlpPosDamgdStress,
     +envlpPosStrain, EXTSTRESS)
                  hghI_TstateStress = EXTSTRESS
                  gammaKUsed = CgammaK
                  P_kElasticNegDamgd = P_kElasticNeg *(ONE - gammaKUsed)
              END IF
          ELSE IF (I_Tstate .EQ. 4) THEN
              IF (SU .GT. hghI_TstateStrain) THEN
                  cis = .true.
                  newState = 1
                  P_lowI_TstateStrain = envlpPosStrain(1)
                  P_lowI_TstateStress = envlpPosDamgdStress(1)
                  hghI_TstateStrain = envlpPosStrain(6)
                  hghI_TstateStress = envlpPosDamgdStress(6)
              ELSE IF (SU .LT. uMinDamgd .AND. SDU .LT. ZERO) THEN
                  cis = .true.
                  newState = 2
                  P_lowI_TstateStrain = envlpNegStrain(6)
                  P_lowI_TstateStress = envlpNegDamgdStress(6)
                  hghI_TstateStrain = envlpNegStrain(1)
                  hghI_TstateStress = envlpNegDamgdStress(1)
              ELSE IF (SDU .LT. ZERO) THEN
                  cis = .true. 
                  newState = 3
                  gammaFUsed = CgammaF        
                  DO I = 1, 6
                      envlpNegDamgdStress(I) = envlpNegStress(I) * 
     +                    (ONE - gammaFUsed)
                  END DO
                  P_lowI_TstateStrain = uMinDamgd
                  CALL negEnvlpStress(uMinDamgd, envlpNegDamgdStress, 
     +                                envlpNegStrain, EXTSTRESS)  
                  P_lowI_TstateStress = EXTSTRESS
C                 SOME BUG HERE
                  hghI_TstateStrain = Cstrain
                  hghI_TstateStress = Cstress
                  gammaKUsed = CgammaK
                  P_kElasticPosDamgd = P_kElasticPos * (ONE-gammaKUsed)
              END IF
          END IF
      END IF           
C 
      IF (cis) THEN
          I_Tstate = newState
      END IF 
C        
      RETURN
      END
C
C
C     ***************************************
C            PINCHING4: posEnvlpStress
C     ***************************************
      SUBROUTINE posEnvlpStress(SU, envlpPosDamgdStress, 
     +envlpPosStrain, EXTSTRESS)
C     
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION envlpPosStrain(6), envlpPosDamgdStress(6)
C
      INTEGER I
      DOUBLE PRECISION SU
      DOUBLE PRECISION P_k, F
      DOUBLE PRECISION EXTSTRESS
C
C
      P_k = ZERO
      F = ZERO
      I = 1
      EXTSTRESS = ZERO
C
      DO WHILE (P_k .EQ. ZERO .AND. I .LE. 5) 
          IF (SU .LE. envlpPosStrain(I+1)) THEN
              P_k = (envlpPosDamgdStress(I+1)-envlpPosDamgdStress(I)) /
     +            (envlpPosStrain(I+1)-envlpPosStrain(I))
              F = envlpPosDamgdStress(I) + (SU-envlpPosStrain(I)) * P_k
          END IF
          I = I + 1  
      END DO
C
      IF (P_k .EQ. ZERO) THEN
          P_k = (envlpPosDamgdStress(6) - envlpPosDamgdStress(5)) /
     +        (envlpPosStrain(6) - envlpPosStrain(5))
          F = envlpPosDamgdStress(6) + P_k * (SU - envlpPosStrain(6))
      END IF
C
      EXTSTRESS = F
C
C      
      RETURN
      END
C
C
C     ***************************************
C            PINCHING4: posEnvlpTangent
C     ***************************************
      SUBROUTINE posEnvlpTangent(SU,envlpPosDamgdStress,envlpPosStrain,
     +                            P_k)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION envlpPosDamgdStress(6), envlpPosStrain(6)
C
      INTEGER I
      DOUBLE PRECISION SU
      DOUBLE PRECISION P_k
C
      P_k = ZERO
      I = 1
      DO WHILE (P_k .EQ. ZERO .AND. I .LT. 5) 
          IF (SU .LE. envlpPosStrain(I+1)) THEN
              P_k = (envlpPosDamgdStress(I+1)-envlpPosDamgdStress(I))/
     +            (envlpPosStrain(I+1)-envlpPosStrain(I))
          END IF
          I = I + 1
      END DO
C
      IF (P_k .EQ. ZERO) THEN
          P_k = (envlpPosDamgdStress(6) - envlpPosDamgdStress(5))/
     +     (envlpPosStrain(6) - envlpPosStrain(5))
      END IF
C
C
      RETURN
      END
C
C
C     ***************************************
C            PINCHING4: negEnvlpStress
C     ***************************************
      SUBROUTINE negEnvlpStress(SU, envlpNegDamgdStress, envlpNegStrain,
     +                          EXTSTRESS)
C     
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION envlpNegStrain(6), envlpNegDamgdStress(6)
C
      INTEGER I
      DOUBLE PRECISION SU
      DOUBLE PRECISION P_k, F
      DOUBLE PRECISION EXTSTRESS
C
      P_k = ZERO
      F = ZERO
      I = 1
      EXTSTRESS = ZERO
C
C
      DO WHILE (P_k .EQ. ZERO .AND. I .LE. 5) 
          IF (SU .GE. envlpNegStrain(I+1)) THEN
              P_k = (envlpNegDamgdStress(I) - envlpNegDamgdStress(I+1))/
     +            (envlpNegStrain(I) - envlpNegStrain(I+1))
              F = envlpNegDamgdStress(I+1) + 
     +            (SU - envlpNegStrain(I+1)) * P_k
          END IF
          I = I + 1  
      END DO
C
      IF (P_k .EQ. ZERO) THEN
          P_k = (envlpNegDamgdStress(5) - envlpNegDamgdStress(6)) /
     +        (envlpNegStrain(5) - envlpNegStrain(6))
          F = envlpNegDamgdStress(6) + P_k * (SU - envlpNegStrain(6))
      END IF
C
      EXTSTRESS = F
C
C      
      RETURN
      END
C
C
C     ***************************************
C            PINCHING4: negEnvlpTangent
C     ***************************************
      SUBROUTINE  negEnvlpTangent(SU,envlpNegDamgdStress,envlpNegStrain,
     +                            P_k)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION envlpNegDamgdStress(6), envlpNegStrain(6)
C
      INTEGER I
      DOUBLE PRECISION SU
      DOUBLE PRECISION P_k
C
      P_k = ZERO
      I = 1
      DO WHILE (P_k .EQ. ZERO .AND. I .LT. 5)
          IF (SU .GE. envlpNegStrain(i+1)) THEN
              P_k = (envlpNegDamgdStress(i)-envlpNegDamgdStress(i+1))/
     +            (envlpNegStrain(i)-envlpNegStrain(i+1))
          END IF
          I = I + 1
      END DO
C
      IF (P_k .EQ. ZERO) THEN
          P_k = (envlpNegDamgdStress(5) - envlpNegDamgdStress(6))/
     +        (envlpNegStrain(5)-envlpNegStrain(6))
      END IF
C
C
      RETURN
      END
C
C
C     ***************************************
C            PINCHING4: getstate3
C     ***************************************
      SUBROUTINE getstate3(state3Strain, state3Stress, P_kunload,
     +    P_kElasticNegDamgd, P_lowI_TstateStrain, P_lowI_TstateStress, 
     +    hghI_TstateStrain, hghI_TstateStress, TminStrainDmnd, 
     +    envlpNegStrain, envlpNegDamgdStress, PROPS)  
C      
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION PROPS(41), 
     +            envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6),
     +            envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state3Strain(4), state3Stress(4),
     +            state4Strain(4), state4Stress(4)
C
C
      LOGICAL cid, cis
C
      INTEGER I
      DOUBLE PRECISION P_kunload
      DOUBLE PRECISION P_kElasticNegDamgd
      DOUBLE PRECISION P_lowI_TstateStrain, P_lowI_TstateStress
      DOUBLE PRECISION hghI_TstateStrain, hghI_TstateStress
      DOUBLE PRECISION TminStrainDmnd
      DOUBLE PRECISION rDispN, rForceN, uForceN
C
      DOUBLE PRECISION P_kmax
      DOUBLE PRECISION st1, st2
      DOUBLE PRECISION STDU, STDF, STDFR
      DOUBLE PRECISION avgforce
      DOUBLE PRECISION slope12, slope34, checkSlope, slope
C
C
C     PARAMETERS TAKEN FROM PROPS 
      rDispN = PROPS(20)
      rForceN = PROPS(21)
      uForceN = PROPS(22)
C
      P_kmax = MAX(P_kunload, P_kElasticNegDamgd)
C     
      IF (state3Strain(1) * state3Strain(4) .LT. ZERO) THEN
          state3Strain(2) = P_lowI_TstateStrain * rDispN
          IF ((rForceN - uForceN) .GT. 1E-8) THEN
              state3Stress(2) = P_lowI_TstateStress * rForceN
          ELSE
              IF (TminStrainDmnd .LT. envlpNegStrain(4)) THEN
                  st1 = P_lowI_TstateStress * uForceN * (ONE+TOL)
                  st2 = envlpNegDamgdStress(5) * (ONE+TOL)
                  state3Stress(2) = MIN(st1, st2)
              ELSE
                  st1 = envlpNegDamgdStress(4) * uForceN * (ONE+TOL)
                  st2 = envlpNegDamgdStress(5) * (ONE+TOL)
                  state3Stress(2) = MIN(st1, st2)
              END IF
          END IF
          IF ((state3Stress(2) - state3Stress(1)) / (state3Strain(2) - 
     +        state3Strain(1)) .GT. P_kElasticNegDamgd) THEN
              state3Strain(2) = P_lowI_TstateStrain + 
     +        (state3Stress(2)-state3Stress(1))/P_kElasticNegDamgd
          END IF
          IF (state3Strain(2) .GT. state3Strain(4)) THEN
              STDU = state3Strain(4) - state3Strain(1)
              STDF = state3Stress(4) - state3Stress(1)
              state3Strain(2) = state3Strain(1) + 0.33D0*STDU
              state3Strain(3) = state3Strain(1) + 0.67D0*STDU
              state3Stress(2) = state3Stress(1) + 0.33D0*STDF
              state3Stress(3) = state3Stress(1) + 0.67D0*STDF
          ELSE 
C             PATH: 
              IF (TminStrainDmnd .LT. envlpNegStrain(4)) THEN
                  state3Stress(3) = uForceN * envlpNegDamgdStress(5)
              ELSE
                  state3Stress(3) = uForceN * envlpNegDamgdStress(4)
C                 PATH:
              END IF
              state3Strain(3) = hghI_TstateStrain - 
     +        (hghI_TstateStress-state3Stress(3))/P_kunload
              IF (state3Strain(3) .GT. state3Strain(4)) THEN
                  STDU = state3Strain(4) - state3Strain(2)
                  STDF = state3Stress(4) - state3Stress(2)
                  state3Strain(3) = state3Strain(2) + HALF*STDU
                  state3Stress(3) = state3Stress(2) + HALF*STDF
              ELSE IF ((state3Stress(3) - state3Stress(2))/
     +            (state3Strain(3) -state3Strain(2)) .GT. P_kmax) THEN
C                     PATH:
                      STDU = state3Strain(4) - state3Strain(1)
                      STDF = state3Stress(4) - state3Stress(1)
                      state3Strain(2) = state3Strain(1) + 0.33D0*STDU
                      state3Strain(3) = state3Strain(1) + 0.67D0*STDU
                      state3Stress(2) = state3Stress(1) + 0.33D0*STDF
                      state3Stress(3) = state3Stress(1) + 0.67D0*STDF
              ELSE IF ((state3Strain(3) .LT. state3Strain(2)) .OR. 
     +            ((state3Stress(3)-state3Stress(2))/
     +            (state3Strain(3)-state3Strain(2)) .LT. 0)) THEN
                          IF (state3Strain(3) .LT. ZERO) THEN
                              STDU = state3Strain(4)-state3Strain(2)
                              STDF = state3Stress(4)-state3Stress(2)
                              state3Strain(3)=state3Strain(2)+HALF*STDU
                              state3Stress(3)= state3Stress(2)+HALF*STDF
                          ELSE IF (state3Strain(2) .GT. ZERO) THEN
                              STDU = state3Strain(3)-state3Strain(1)
                              STDF = state3Stress(3)-state3Stress(1)
                              state3Strain(2)=state3Strain(1)+HALF*STDU
                              state3Stress(2)=state3Stress(1)+HALF*STDF
                          ELSE
                              avgforce = HALF*(state3Stress(3) + 
     +                        state3Stress(2))
                              STDFR = ZERO
                              IF (avgforce .LT. ZERO) THEN
                                  STDFR = -avgforce/100.0D0
                              ELSE
                                  STDFR = avgforce/100.0D0
                              END IF
                              slope12 = (state3Stress(2) - 
     +            state3Stress(1))/(state3Strain(2) - state3Strain(1))
                              slope34 = (state3Stress(4) - 
     +            state3Stress(3))/(state3Strain(4) - state3Strain(3))
                              state3Stress(2) = avgforce - STDFR
                              state3Stress(3) = avgforce + STDFR
                              state3Strain(2) = state3Strain(1) + 
     +            (state3Stress(2) - state3Stress(1))/slope12
                              state3Strain(3) = state3Strain(4) - 
     +            (state3Stress(4) - state3Stress(3))/slope34
                          END IF
              END IF
          END IF
      ELSE
          STDU = state3Strain(4)-state3Strain(1)
          STDF = state3Stress(4)-state3Stress(1)
          state3Strain(2) = state3Strain(1) + 0.33D0*STDU;
          state3Strain(3) = state3Strain(1) + 0.67D0*STDU;
	    state3Stress(2) = state3Stress(1) + 0.33D0*STDF;
	    state3Stress(3) = state3Stress(1) + 0.67D0*STDF;
      END IF
C
      checkSlope = state3Stress(1)/state3Strain(1)
      slope = ZERO
C     
C     FINAL CHECK
      I = 1
      DO WHILE (I .LT. 4)
          STDU = state3Strain(i+1)-state3Strain(i)
          STDF = state3Stress(i+1)-state3Stress(i)
          IF (STDU .LT. ZERO .OR. STDF .LT. ZERO) THEN
              STDU = state3Strain(4)-state3Strain(1)
              STDF = state3Stress(4)-state3Stress(1)
		    state3Strain(2) = state3Strain(1) + 0.33D0*STDU
		    state3Strain(3) = state3Strain(1) + 0.67D0*STDU
		    state3Stress(2) = state3Stress(1) + 0.33D0*STDF
		    state3Stress(3) = state3Stress(1) + 0.67D0*STDF
              slope = STDF / STDU
              I = 4
          END IF
          IF (slope .GT. 1.0D-8 .AND. slope .LT. checkSlope) THEN
              state3Strain(2) = ZERO; 
              state3Stress(2) = ZERO;
		    state3Strain(3) = state3Strain(4)*HALF
              state3Stress(3) = state3Stress(4)*HALF
          END IF
          I = I + 1
      END DO
C
C
      RETURN
      END
C
C
C     ***************************************
C             PINCHING4: getstate4
C     ***************************************
      SUBROUTINE getstate4(state4Strain, state4Stress, P_kunload,
     +    P_kElasticPosDamgd, P_lowI_TstateStrain,P_lowI_TstateStress,
     +    hghI_TstateStrain, hghI_TstateStress, TmaxStrainDmnd, 
     +    envlpPosStrain, envlpPosDamgdStress, PROPS)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION PROPS(41), 
     +            envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6),
     +            envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state4Strain(4), state4Stress(4)
C
C
      LOGICAL cid, cis
C
      INTEGER I
      DOUBLE PRECISION P_kunload
      DOUBLE PRECISION P_kElasticPosDamgd
      DOUBLE PRECISION P_lowI_TstateStrain,P_lowI_TstateStress
      DOUBLE PRECISION hghI_TstateStrain, hghI_TstateStress
      DOUBLE PRECISION TmaxStrainDmnd
      DOUBLE PRECISION rDispP, rForceP, uForceP
C
      DOUBLE PRECISION P_kmax
      DOUBLE PRECISION st1, st2
      DOUBLE PRECISION STDU, STDF, STDFR
      DOUBLE PRECISION avgforce
      DOUBLE PRECISION slope12, slope34, checkSlope, slope
      
C
C
C
C     PARAMETERS TAKEN FROM PROPS 
      rDispP = PROPS(17)
      rForceP = PROPS(18)
      uForceP = PROPS(19)
C
      P_kmax = MAX(P_kunload, P_kElasticPosDamgd)
C     
      IF (state4Strain(1) * state4Strain(4) .LT. ZERO) THEN
          state4Strain(3) = hghI_TstateStrain * rDispP
          IF (uForceP .EQ. ZERO) THEN
              state4Stress(3) = hghI_TstateStress * rForceP
          ELSE IF (rForceP-uForceP .GT. 1.0D-8) THEN
              state4Stress(3) = hghI_TstateStress * rForceP
          ELSE
              IF (TmaxStrainDmnd .GT. envlpPosStrain(4)) THEN
                  st1 = hghI_TstateStress * uForceP * (ONE+TOL)
                  st2 = envlpNegDamgdStress(5) * (ONE+TOL)
                  state4Stress(3) = MAX(st1, st2)
              ELSE
                  st1 = envlpPosDamgdStress(4)*uForceP*(ONE+TOL)
                  st2 = envlpPosDamgdStress(5)*(ONE+TOL)
                  state4Stress(3) = MIN(st1, st2)
              END IF
          END IF
          IF ((state4Stress(4) - state4Stress(3)) / (state4Strain(4) - 
     +        state4Strain(3)) .GT. P_kElasticPosDamgd) THEN
              state4Strain(3) = hghI_TstateStrain - 
     +        (state4Stress(4)-state4Stress(3))/P_kElasticPosDamgd
          END IF
          IF (state4Strain(3) .LT. state4Strain(1)) THEN
              STDU = state4Strain(4) - state4Strain(1)
              STDF = state4Stress(4) - state4Stress(1)
              state4Strain(2) = state4Strain(1) + 0.33D0*STDU
              state4Strain(3) = state4Strain(1) + 0.67D0*STDU
              state4Stress(2) = state4Stress(1) + 0.33D0*STDF
              state4Stress(3) = state4Stress(1) + 0.67D0*STDF
          ELSE 
C             PATH: 
              IF (TmaxStrainDmnd .GT. envlpPosStrain(4)) THEN
                  state4Stress(2) = uForceP * envlpPosDamgdStress(5)
              ELSE
                  state4Stress(2) = uForceP * envlpPosDamgdStress(4)
C                 PATH:
              END IF
              state4Strain(2) = P_lowI_TstateStrain + 
     +        (-P_lowI_TstateStress + state4Stress(2))/P_kunload
              IF (state4Strain(2) .LT. state4Strain(1)) THEN
                  STDU = state4Strain(3) - state4Strain(1)
                  STDF = state4Stress(3) - state4Stress(1)
                  state4Strain(2) = state4Strain(1) + HALF*STDU
                  state4Stress(2) = state4Stress(1) + HALF*STDF
              ELSE IF ((state4Stress(3) - state4Stress(2))/
     +            (state4Strain(3) -state4Strain(2)) .GT. P_kmax) THEN
C                     PATH:
                      STDU = state4Strain(4) - state4Strain(1)
                      STDF = state4Stress(4) - state4Stress(1)
                      state4Strain(2) = state4Strain(1) + 0.33D0*STDU
                      state4Strain(3) = state4Strain(1) + 0.67D0*STDU
                      state4Stress(2) = state4Stress(1) + 0.33D0*STDF
                      state4Stress(3) = state4Stress(1) + 0.67D0*STDF
              ELSE IF ((state4Strain(3) .LT. state4Strain(2)) .OR. 
     +            ((state4Stress(3)-state4Stress(2))/
     +            (state4Strain(3)-state4Strain(2)) .LT. ZERO)) THEN
                          IF (state4Strain(2) .GT. ZERO) THEN
                              STDU = state4Strain(3)-state4Strain(1)
                              STDF = state4Stress(3)-state4Stress(1)
                              state4Strain(2)=state4Strain(1)+HALF*STDU
                              state4Stress(2)=state4Stress(1)+HALF*STDF
                          ELSE IF (state4Strain(3) .LT. ZERO) THEN
                              STDU = state4Strain(4)-state4Strain(2)
                              STDF = state4Stress(4)-state4Stress(2)
                              state4Strain(3)=state4Strain(2)+HALF*STDU
                              state4Stress(3)=state4Stress(2)+HALF*STDF
                          ELSE
                              avgforce = HALF*(state4Stress(3) + 
     +                        state4Stress(2))
                              STDFR = ZERO
                              IF (avgforce .LT. ZERO) THEN
                                  STDFR = -avgforce/100.0D0
                              ELSE
                                  STDFR = avgforce/100.0D0
                              END IF
                              slope12 = (state4Stress(2) - 
     +            state4Stress(1))/(state4Strain(2) - state4Strain(1))
                              slope34 = (state4Stress(4) - 
     +            state4Stress(3))/(state4Strain(4) - state4Strain(3))
                              state4Stress(2) = avgforce - STDFR
                              state4Stress(3) = avgforce + STDFR
                              state4Strain(2) = state4Strain(1) + 
     +            (state4Stress(2) - state4Stress(1))/slope12
                              state4Strain(3) = state4Strain(4) - 
     +            (state4Stress(4) - state4Stress(3))/slope34
                          END IF
              END IF
          END IF
      ELSE
          STDU = state4Strain(4)-state4Strain(1)
          STDF = state4Stress(4)-state4Stress(1)
          state4Strain(2) = state4Strain(1) + 0.33D0*STDU;
          state4Strain(3) = state4Strain(1) + 0.67D0*STDU;
	    state4Stress(2) = state4Stress(1) + 0.33D0*STDF;
	    state4Stress(3) = state4Stress(1) + 0.67D0*STDF;
      END IF
C
      checkSlope = state4Stress(1)/state4Strain(1)
      slope = ZERO
C     
C     FINAL CHECK
      I = 1
      DO WHILE (I .LT. 4)
          STDU = state4Strain(I+1)-state4Strain(I)
          STDF = state4Stress(I+1)-state4Stress(I)
          IF (STDU .LT. ZERO .OR. STDF .LT. ZERO) THEN
              STDU = state4Strain(4)-state4Strain(1)
              STDF = state4Stress(4)-state4Stress(1)
		    state4Strain(2) = state4Strain(1) + 0.33D0*STDU
		    state4Strain(3) = state4Strain(1) + 0.67D0*STDU
		    state4Stress(2) = state4Stress(1) + 0.33D0*STDF
		    state4Stress(3) = state4Stress(1) + 0.67D0*STDF
              slope = STDF / STDU
              I = 4
          END IF
          IF (slope .GT. 1.0D-8 .AND. slope .LT. checkSlope) THEN
              state4Strain(2) = ZERO; 
              state4Stress(2) = ZERO;
		    state4Strain(3) = state4Strain(4)*HALF
              state4Stress(3) = state4Stress(4)*HALF
          END IF
          I = I + 1
      END DO
C
C
      RETURN
      END
C
C
C     ***************************************
C             PINCHING4: Envlp3Stress
C     ***************************************
      SUBROUTINE Envlp3Stress(s3Strain,s3Stress,SU, f)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION s3Strain(4), s3Stress(4)
C     
      INTEGER I
      DOUBLE PRECISION SU
      DOUBLE PRECISION P_k, f 
C
      P_k = ZERO
      I = 1
      f = ZERO
      DO WHILE ((P_k .EQ. ZERO .OR. i .LE. 3) .AND. i .LE. 3)
          IF (SU .GE. s3Strain(i)) THEN
              P_k = (s3Stress(i+1)-s3Stress(i))/
     +            (s3Strain(i+1)-s3Strain(i))
              f =  s3Stress(i)+(SU-s3Strain(i))*P_k
          END IF
          I = I + 1
      END DO
      IF (P_k .EQ. ZERO) THEN
          IF (SU .LT. s3Strain(1)) THEN
              I = 1
          ELSE
              I = 3
          END IF
	    P_k = (s3Stress(i+1)-s3Stress(i))/(s3Strain(i+1)-s3Strain(i))
	    f = s3Stress(i)+(SU-s3Strain(i))*P_k
      END IF
C
C
      RETURN
      END
C
C
C     ***************************************
C             PINCHING4: Envlp3Tangent
C     ***************************************
      SUBROUTINE Envlp3Tangent(s3Strain,s3Stress,SU, P_k)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION s3Strain(4), s3Stress(4)
C
      INTEGER I
      DOUBLE PRECISION SU
      DOUBLE PRECISION P_k
C
      P_k = ZERO
      I = 1
      DO WHILE ((P_k .EQ. ZERO .OR. i .LE. 3) .AND. i .LE. 3)
          IF (SU .GE. s3Strain(i)) THEN
              P_k = (s3Stress(i+1)-s3Stress(i))/
     +            (s3Strain(i+1)-s3Strain(i))
          END IF
          I = I + 1
      END DO
      IF (P_k .EQ. ZERO) THEN
          IF (SU .LT. s3Strain(1)) THEN
              I = 1
          ELSE 
              I = 3
          END IF
          P_k = (s3Stress(i+1)-s3Stress(i))/(s3Strain(i+1)-s3Strain(i))
      END IF
C
C
      RETURN 
      END
C
C
C     ***************************************
C             PINCHING4: Envlp4Stress
C     ***************************************
      SUBROUTINE Envlp4Stress(s4Strain,s4Stress,SU, f)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION s4Strain(4), s4Stress(4)
C
      INTEGER I
      DOUBLE PRECISION SU
      DOUBLE PRECISION P_k, f
C
      P_k = ZERO
      I = 1
      f = ZERO
      DO WHILE ((P_k .EQ. ZERO .OR. i .LE. 3) .AND. i .LE. 3)
          IF (SU .GE. s4Strain(i)) THEN
              P_k = (s4Stress(i+1)-s4Stress(i))/
     +            (s4Strain(i+1)-s4Strain(i))
              f =  s4Stress(i)+(SU-s4Strain(i))*P_k
          END IF
          I = I + 1
      END DO
      IF (P_k .EQ. ZERO) THEN
          IF (SU .LT. s4Strain(1)) THEN
              I = 1
          ELSE
              I = 3
          END IF
	    P_k = (s4Stress(i+1)-s4Stress(i))/(s4Strain(i+1)-s4Strain(i))
	    f = s4Stress(i)+(SU-s4Strain(i))*P_k
      END IF
C
C
      RETURN
      END
C
C
C     ***************************************
C             PINCHING4: Envlp4Tangent
C     ***************************************
      SUBROUTINE Envlp4Tangent(s4Strain,s4Stress, SU, P_k)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION s4Strain(4), s4Stress(4)
C  
      INTEGER I
      DOUBLE PRECISION SU
      DOUBLE PRECISION P_k
C
      P_k = ZERO
      I = 1
      DO WHILE ((P_k .EQ. ZERO .OR. i .LE. 3) .AND. (i .LE. 3))
          IF (SU .GE. s4Strain(i)) THEN
              P_k = (s4Stress(i+1)-s4Stress(i))/
     +            (s4Strain(i+1)-s4Strain(i))
          END IF
          I = I + 1
      END DO
      IF (P_k .EQ. ZERO) THEN
          IF (SU .LT. s4Strain(1)) THEN
              I = 1
          ELSE 
              I = 3
          END IF
          P_k = (s4Stress(i+1)-s4Stress(i))/(s4Strain(i+1)-s4Strain(i))
      END IF
C
C
      RETURN
      END
C
C
C     ***************************************
C              PINCHING4: updateDmg
C     ***************************************
      SUBROUTINE updateDmg(strain, dstrain, 
     +            TmaxStrainDmnd, TminStrainDmnd,
     +            envlpPosStrain, envlpNegStrain,
     +            envlpPosDamgdStress,envlpNegDamgdStress,
     +            CnCycle,Tenergy,energyCapacity,
     +            I_DmgCyc, elasticStrainEnergy,
     +            P_kElasticPos,P_kElasticNeg,
     +            TnCycle,TgammaK,TgammaD,TgammaF,
     +            PROPS)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION PROPS(41), 
     +            envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6),
     +            envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state3Strain(4), state3Stress(4),
     +            state4Strain(4), state4Stress(4)
C
      INTEGER I_DmgCyc
      DOUBLE PRECISION strain, dstrain
      DOUBLE PRECISION TmaxStrainDmnd, TminStrainDmnd
      DOUBLE PRECISION P_kElasticPos,P_kElasticNeg
      DOUBLE PRECISION CnCycle, TnCycle
      DOUBLE PRECISION Tenergy, energyCapacity, elasticStrainEnergy
      DOUBLE PRECISION TgammaK,TgammaD,TgammaF
C
      DOUBLE PRECISION gammaK1, gammaK2, gammaK3, gammaK4, gammaKLimit
      DOUBLE PRECISION gammaD1, gammaD2, gammaD3, gammaD4, gammaDLimit
      DOUBLE PRECISION gammaF1, gammaF2, gammaF3, gammaF4, gammaFLimit
C
      DOUBLE PRECISION tes
      DOUBLE PRECISION umaxAbs, uultAbs
      DOUBLE PRECISION gammaKLimEnv 
C
      DOUBLE PRECISION EXTSTRESS
      DOUBLE PRECISION P_kminP, P_kminN, P_kmin
      DOUBLE PRECISION P_k1
      
C
C     UNZIP PARAMETERS FROM PROPS()
      gammaK1 = PROPS(23)
      gammaK2 = PROPS(24)
      gammaK3 = PROPS(25)
      gammaK4 = PROPS(26)
      gammaKLimit = PROPS(27)
      gammaD1 = PROPS(28)
      gammaD2 = PROPS(29)
      gammaD3 = PROPS(30)
      gammaD4 = PROPS(31)
      gammaDLimit = PROPS(32)      
      gammaF1 = PROPS(33)
      gammaF2 = PROPS(34)
      gammaF3 = PROPS(35)
      gammaF4 = PROPS(36)
      gammaFLimit = PROPS(37)
      I_DmgCyc = INT(PROPS(39))
C
      tes = ZERO
      umaxAbs = MAX(TmaxStrainDmnd, -TminStrainDmnd)
      uultAbs = MAX(envlpPosStrain(5), -envlpNegStrain(5))
      TnCycle = CnCycle + DABS(dstrain)/(4.0D0*umaxAbs)
C
      IF ((strain .LT. uultAbs .AND. strain .GT. -uultAbs) .AND. 
     + Tenergy .LT. energyCapacity) THEN
          TgammaK = gammaK1 * ((umaxAbs/uultAbs) ** gammaK3)
          TgammaD = gammaD1 * ((umaxAbs/uultAbs) ** gammaD3)
          TgammaF = gammaF1 * ((umaxAbs/uultAbs) ** gammaF3)
C
          IF (Tenergy .GT. elasticStrainEnergy 
     + .AND. I_DmgCyc .EQ. 0) THEN
              tes = (Tenergy-elasticStrainEnergy)/energyCapacity
              TgammaK = TgammaK + gammaK2 * (tes ** gammaK4)
              TgammaD = TgammaD + gammaD2 * (tes ** gammaD4)
              TgammaF = TgammaF + gammaF2 * (tes ** gammaF4)
          ELSE IF (I_DmgCyc .EQ. 1) THEN
              TgammaK = TgammaK + gammaK2 * (TnCycle ** gammaK4)
              TgammaD = TgammaD + gammaD2 * (TnCycle ** gammaD4)
              TgammaF = TgammaF + gammaF2 * (TnCycle ** gammaF4)
          END IF
C
          CALL posEnvlpStress(TmaxStrainDmnd, envlpPosDamgdStress, 
     +        envlpPosStrain, EXTSTRESS)
          P_kminP = EXTSTRESS/TmaxStrainDmnd
          CALL negEnvlpStress(TminStrainDmnd, envlpNegDamgdStress, 
     +        envlpNegStrain, EXTSTRESS)
          P_kminN = EXTSTRESS/TminStrainDmnd
          P_kmin = MAX(P_kminP/P_kElasticPos, P_kminN/P_kElasticNeg)
          gammaKLimEnv = MAX(ZERO, (ONE-P_kmin))
C
          P_k1 = MIN(TgammaK, gammaKLimit)
          TgammaK = MIN(P_k1, gammaKLimEnv)
          TgammaD = MIN(TgammaD, gammaDLimit)
          TgammaF = MIN(TgammaF, gammaFLimit)
      ELSE IF (strain .LT. uultAbs .AND. strain .GT.-uultAbs) THEN
          CALL posEnvlpStress(TmaxStrainDmnd, envlpPosDamgdStress, 
     +        envlpPosStrain, EXTSTRESS)
          P_kminP = EXTSTRESS/TmaxStrainDmnd
          CALL negEnvlpStress(TminStrainDmnd, envlpNegDamgdStress, 
     +        envlpNegStrain, EXTSTRESS)
          P_kminN = EXTSTRESS/TminStrainDmnd  
          P_kmin = MAX(P_kminP/P_kElasticPos, P_kminN/P_kElasticNeg)
          gammaKLimEnv = MAX(ZERO, (ONE-P_kmin))
C
          TgammaK = MIN(gammaKLimit, gammaKLimEnv)
          TgammaD = MIN(TgammaD, gammaDLimit)
          TgammaF = MIN(TgammaF, gammaFLimit)
C
      END IF          
C
C
      RETURN
      END
C
C
C     ***************************************
C              PINCHING4: commitState
C     ***************************************
      SUBROUTINE commitState(I_Tstate,dstrain,TstrainRate,
     +                P_lowI_TstateStrain,P_lowI_TstateStress,
     +                hghI_TstateStrain,hghI_TstateStress,
     +                TminStrainDmnd,TmaxStrainDmnd,
     +                Tenergy,Tstress,Tstrain,
     +                TgammaK,TgammaD,TgammaF,
     +                P_kElasticPos,P_kElasticNeg,
     +                gammaKUsed,gammaFUsed,
     +                envlpPosStress,envlpNegStress,TnCycle,
     +                I_Cstate,CstrainRate,
     +                P_lowI_CstateStrain,P_lowI_CstateStress,
     +                hghI_CstateStrain,hghI_CstateStress,
     +                CminStrainDmnd,CmaxStrainDmnd,
     +                Cenergy,Cstress,Cstrain,
     +                CgammaK,CgammaD,CgammaF,
     +                P_kElasticPosDamgd,P_kElasticNegDamgd,
     +                uMaxDamgd,uMinDamgd,
     +                envlpPosDamgdStress,envlpNegDamgdStress,CnCycle)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION PROPS(41), 
     +            envlpPosStrain(6), envlpPosStress(6),
     +            envlpNegStrain(6), envlpNegStress(6),
     +            envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state3Strain(4), state3Stress(4),
     +            state4Strain(4), state4Stress(4)
C
C
      INTEGER I_Cstate, I_Tstate
      DOUBLE PRECISION dstrain
      DOUBLE PRECISION Cstrain, Cstress, CstrainRate
      DOUBLE PRECISION Tstrain, Tstress, TstrainRate
      DOUBLE PRECISION P_lowI_TstateStrain, P_lowI_TstateStress
      DOUBLE PRECISION hghI_TstateStrain, hghI_TstateStress
      DOUBLE PRECISION P_lowI_CstateStrain,P_lowI_CstateStress
      DOUBLE PRECISION hghI_CstateStrain,hghI_CstateStress
      DOUBLE PRECISION TminStrainDmnd,TmaxStrainDmnd
      DOUBLE PRECISION CminStrainDmnd,CmaxStrainDmnd
      DOUBLE PRECISION Cenergy, Tenergy
      DOUBLE PRECISION CnCycle, TnCycle
      DOUBLE PRECISION TgammaK,TgammaD,TgammaF
      DOUBLE PRECISION CgammaK,CgammaD,CgammaF
      DOUBLE PRECISION gammaKUsed,gammaFUsed
      DOUBLE PRECISION P_kElasticPos,P_kElasticNeg
      DOUBLE PRECISION P_kElasticPosDamgd,P_kElasticNegDamgd
      DOUBLE PRECISION uMaxDamgd,uMinDamgd
C
      I_Cstate = I_Tstate
C
      IF (dstrain .GT. 1.0D-12 .OR. dstrain<-(1.0D-12)) THEN
          CstrainRate = dstrain
      ELSE 
          CstrainRate = TstrainRate
      END IF
C
      P_lowI_CstateStrain = P_lowI_TstateStrain
      P_lowI_CstateStress = P_lowI_TstateStress
      hghI_CstateStrain = hghI_TstateStrain
      hghI_CstateStress = hghI_TstateStress
      CminStrainDmnd = TminStrainDmnd
      CmaxStrainDmnd = TmaxStrainDmnd
      Cenergy = Tenergy
C
      Cstress = Tstress
      Cstrain = Tstrain
C
      CgammaK = TgammaK
      CgammaD = TgammaD
      CgammaF = TgammaF
C
      P_kElasticPosDamgd = P_kElasticPos*(1 - gammaKUsed)
      P_kElasticNegDamgd = P_kElasticNeg*(1 - gammaKUsed)
C
      uMaxDamgd = TmaxStrainDmnd*(1 + CgammaD)
      uMinDamgd = TminStrainDmnd*(1 + CgammaD)
C
      envlpPosDamgdStress = envlpPosStress*(1-gammaFUsed)
      envlpNegDamgdStress = envlpNegStress*(1-gammaFUsed)
C
      CnCycle = TnCycle
C
C
      RETURN
      END
C
C
C
C
C     ***************************************
C         PINCHING4 INTERFACE:SPIN2UEL
C     ***************************************
      SUBROUTINE SPIN2UEL(envlpPosDamgdStress, envlpNegDamgdStress,
     +state3Strain, state3Stress, state4Strain, state4Stress,
     +I_Cstate, Cstrain, Cstress, CstrainRate, P_lowI_CstateStrain,
     +P_lowI_CstateStress, hghI_CstateStrain, hghI_CstateStress, 
     +CminStrainDmnd, CmaxStrainDmnd, Cenergy, CgammaK,
     +CgammaD, CgammaF, Ttangent, gammaKUsed, gammaFUsed,
     +P_kElasticPosDamgd, P_kElasticNegDamgd, uMaxDamgd,uMinDamgd,
     +P_kunload, CnCycle, elasticStrainEnergy, strain, dstrain, 
     +SPR_DISP, SPR_F, SPR_K, SVARS, I_SPR_NUM)
C
C
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state3Strain(4), state3Stress(4),
     +            state4Strain(4), state4Stress(4), 
     +            SVARS(200)
C
      DOUBLE PRECISION strain, dstrain
      DOUBLE PRECISION Cstrain, Cstress, CstrainRate
      DOUBLE PRECISION P_lowI_CstateStrain, P_lowI_CstateStress
      DOUBLE PRECISION hghI_CstateStrain, hghI_CstateStress
      DOUBLE PRECISION CminStrainDmnd, CmaxStrainDmnd
      DOUBLE PRECISION Cenergy
      DOUBLE PRECISION CgammaK, CgammaD, CgammaF
      DOUBLE PRECISION Ttangent
      DOUBLE PRECISION gammaKUsed, gammaFUsed
      DOUBLE PRECISION P_kElasticPosDamgd, P_kElasticNegDamgd
      DOUBLE PRECISION uMaxDamgd,uMinDamgd
      DOUBLE PRECISION P_kunload
      DOUBLE PRECISION CnCycle
      DOUBLE PRECISION elasticStrainEnergy
      DOUBLE PRECISION SPR_DISP, SPR_K, SPR_F
C
      INTEGER I_Cstate
C
      INTEGER I_SPR_NUM
C
C
      IF (I_SPR_NUM .EQ. 1) THEN
          SVARS(1) = envlpPosDamgdStress(1)
          SVARS(2) = envlpPosDamgdStress(2)
          SVARS(3) = envlpPosDamgdStress(3)
          SVARS(4) = envlpPosDamgdStress(4)
          SVARS(5) = envlpPosDamgdStress(5)
          SVARS(6) = envlpPosDamgdStress(6)
          SVARS(7) = envlpNegDamgdStress(1)
          SVARS(8) = envlpNegDamgdStress(2)
          SVARS(9) = envlpNegDamgdStress(3)
          SVARS(10) = envlpNegDamgdStress(4)
          SVARS(11) = envlpNegDamgdStress(5)
          SVARS(12) = envlpNegDamgdStress(6)
          SVARS(13) = state3Strain(1)
          SVARS(14) = state3Strain(2)
          SVARS(15) = state3Strain(3)
          SVARS(16) = state3Strain(4)
          SVARS(17) = state3Stress(1)
          SVARS(18) = state3Stress(2)
          SVARS(19) = state3Stress(3)
          SVARS(20) = state3Stress(4)
          SVARS(21) = state4Strain(1)
          SVARS(22) = state4Strain(2)
          SVARS(23) = state4Strain(3)
          SVARS(24) = state4Strain(4)
          SVARS(25) = state4Stress(1)
          SVARS(26) = state4Stress(2)
          SVARS(27) = state4Stress(3)
          SVARS(28) = state4Stress(4)
          SVARS(29) = DBLE(I_Cstate)
          SVARS(30) = Cstrain
          SVARS(31) = Cstress
          SVARS(32) = CstrainRate
          SVARS(33) = P_lowI_CstateStrain
          SVARS(34) = P_lowI_CstateStress
          SVARS(35) = hghI_CstateStrain
          SVARS(36) = hghI_CstateStress
          SVARS(37) = CminStrainDmnd
          SVARS(38) = CmaxStrainDmnd
          SVARS(39) = Cenergy
          SVARS(40) = CgammaK
          SVARS(41) = CgammaD
          SVARS(42) = CgammaF
          SVARS(43) = gammaKUsed
          SVARS(44) = gammaFUsed
          SVARS(45) = Ttangent
          SVARS(46) = P_kElasticPosDamgd
          SVARS(47) = P_kElasticNegDamgd
          SVARS(48) = uMaxDamgd
          SVARS(49) = uMinDamgd
          SVARS(50) = P_kunload
          SVARS(51) = CnCycle
          SVARS(52) = elasticStrainEnergy
          SVARS(53) = strain
          SVARS(54) = dstrain
          SVARS(55) = SPR_DISP
          SVARS(56) = SPR_F
          SVARS(57) = SPR_K
      ELSE IF (I_SPR_NUM .EQ. 2) THEN
          SVARS(58) = envlpPosDamgdStress(1)
          SVARS(59) = envlpPosDamgdStress(2)
          SVARS(60) = envlpPosDamgdStress(3)
          SVARS(61) = envlpPosDamgdStress(4)
          SVARS(62) = envlpPosDamgdStress(5)
          SVARS(63) = envlpPosDamgdStress(6)
          SVARS(64) = envlpNegDamgdStress(1)
          SVARS(65) = envlpNegDamgdStress(2)
          SVARS(66) = envlpNegDamgdStress(3)
          SVARS(67) = envlpNegDamgdStress(4)
          SVARS(68) = envlpNegDamgdStress(5)
          SVARS(69) = envlpNegDamgdStress(6)
          SVARS(70) = state3Strain(1)
          SVARS(71) = state3Strain(2)
          SVARS(72) = state3Strain(3)
          SVARS(73) = state3Strain(4)
          SVARS(74) = state3Stress(1)
          SVARS(75) = state3Stress(2)
          SVARS(76) = state3Stress(3)
          SVARS(77) = state3Stress(4)
          SVARS(78) = state4Strain(1)
          SVARS(79) = state4Strain(2)
          SVARS(80) = state4Strain(3)
          SVARS(81) = state4Strain(4)
          SVARS(82) = state4Stress(1)
          SVARS(83) = state4Stress(2)
          SVARS(84) = state4Stress(3)
          SVARS(85) = state4Stress(4)
          SVARS(86) = DBLE(I_Cstate)
          SVARS(87) = Cstrain
          SVARS(88) = Cstress
          SVARS(89) = CstrainRate
          SVARS(90) = P_lowI_CstateStrain
          SVARS(91) = P_lowI_CstateStress
          SVARS(92) = hghI_CstateStrain
          SVARS(93) = hghI_CstateStress
          SVARS(94) = CminStrainDmnd
          SVARS(95) = CmaxStrainDmnd
          SVARS(96) = Cenergy
          SVARS(97) = CgammaK
          SVARS(98) = CgammaD
          SVARS(99) = CgammaF
          SVARS(100) = gammaKUsed
          SVARS(101) = gammaFUsed
          SVARS(102) = Ttangent
          SVARS(103) = P_kElasticPosDamgd
          SVARS(104) = P_kElasticNegDamgd
          SVARS(105) = uMaxDamgd
          SVARS(106) = uMinDamgd
          SVARS(107) = P_kunload
          SVARS(108) = CnCycle
          SVARS(109) = elasticStrainEnergy
          SVARS(110) = strain
          SVARS(111) = dstrain
          SVARS(112) = SPR_DISP
          SVARS(113) = SPR_F
          SVARS(114) = SPR_K
      END IF
C
C
      RETURN
      END
C
C
C     ***************************************
C         PINCHING4 INTERFACE:SUEL2PIN
C     ***************************************
      SUBROUTINE SUEL2PIN(envlpPosDamgdStress, envlpNegDamgdStress,
     +state3Strain, state3Stress, state4Strain, state4Stress,
     +I_Cstate, Cstrain, Cstress, CstrainRate, P_lowI_CstateStrain,
     +P_lowI_CstateStress, hghI_CstateStrain, hghI_CstateStress, 
     +CminStrainDmnd, CmaxStrainDmnd, Cenergy, CgammaK,
     +CgammaD, CgammaF, Ttangent, gammaKUsed, gammaFUsed,
     +P_kElasticPosDamgd, P_kElasticNegDamgd, uMaxDamgd,uMinDamgd,
     +P_kunload, CnCycle, elasticStrainEnergy, 
     +SPR_F, SPR_K, SVARS, I_SPR_NUM)
C
C 
      INCLUDE 'ABA_PARAM.INC'
C
      PARAMETER (TOL = 1.D-128, ZERO = 0.D0, PONE = 0.1D0, HALF = 0.5D0,
     + ONE = 1.D0)
C
      DIMENSION envlpPosDamgdStress(6), envlpNegDamgdStress(6),
     +            state3Strain(4), state3Stress(4),
     +            state4Strain(4), state4Stress(4), SVARS(200)
C
      DOUBLE PRECISION strain, dstrain
      DOUBLE PRECISION Cstrain, Cstress, CstrainRate
      DOUBLE PRECISION P_lowI_CstateStrain, P_lowI_CstateStress
      DOUBLE PRECISION hghI_CstateStrain, hghI_CstateStress
      DOUBLE PRECISION CminStrainDmnd, CmaxStrainDmnd
      DOUBLE PRECISION Cenergy
      DOUBLE PRECISION CgammaK, CgammaD, CgammaF
      DOUBLE PRECISION Ttangent
      DOUBLE PRECISION gammaKUsed, gammaFUsed
      DOUBLE PRECISION P_kElasticPosDamgd, P_kElasticNegDamgd
      DOUBLE PRECISION uMaxDamgd,uMinDamgd
      DOUBLE PRECISION P_kunload
      DOUBLE PRECISION CnCycle
      DOUBLE PRECISION elasticStrainEnergy
      DOUBLE PRECISION SPR_DISP, SPR_K, SPR_F
C
      INTEGER I_Cstate
C
      INTEGER I_SPR_NUM
C
      IF (I_SPR_NUM .EQ. 1) THEN
          envlpPosDamgdStress(1) = SVARS(1)
          envlpPosDamgdStress(2) = SVARS(2) 
          envlpPosDamgdStress(3) = SVARS(3) 
          envlpPosDamgdStress(4) = SVARS(4)
          envlpPosDamgdStress(5) = SVARS(5)
          envlpPosDamgdStress(6) = SVARS(6)
          envlpNegDamgdStress(1) = SVARS(7)
          envlpNegDamgdStress(2) = SVARS(8) 
          envlpNegDamgdStress(3) = SVARS(9) 
          envlpNegDamgdStress(4) = SVARS(10)
          envlpNegDamgdStress(5) = SVARS(11)
          envlpNegDamgdStress(6) = SVARS(12) 
          state3Strain(1) = SVARS(13) 
          state3Strain(2) = SVARS(14) 
          state3Strain(3) = SVARS(15) 
          state3Strain(4) = SVARS(16) 
          state3Stress(1) = SVARS(17) 
          state3Stress(2) = SVARS(18) 
          state3Stress(3) = SVARS(19) 
          state3Stress(4) = SVARS(20) 
          state4Strain(1) = SVARS(21) 
          state4Strain(2) = SVARS(22) 
          state4Strain(3) = SVARS(23) 
          state4Strain(4) = SVARS(24) 
          state4Stress(1) = SVARS(25)
          state4Stress(2) = SVARS(26) 
          state4Stress(3) = SVARS(27) 
          state4Stress(4) = SVARS(28) 
          I_Cstate = INT(SVARS(29))
          Cstrain = SVARS(30) 
          Cstress = SVARS(31) 
          CstrainRate = SVARS(32) 
          P_lowI_CstateStrain = SVARS(33) 
          P_lowI_CstateStress = SVARS(34) 
          hghI_CstateStrain = SVARS(35) 
          hghI_CstateStress = SVARS(36) 
          CminStrainDmnd = SVARS(37) 
          CmaxStrainDmnd = SVARS(38) 
          Cenergy = SVARS(39) 
          CgammaK = SVARS(40) 
          CgammaD = SVARS(41) 
          CgammaF = SVARS(42) 
          gammaKUsed = SVARS(43) 
          gammaFUsed = SVARS(44) 
          Ttangent = SVARS(45) 
          P_kElasticPosDamgd = SVARS(46) 
          P_kElasticNegDamgd = SVARS(47) 
          uMaxDamgd = SVARS(48) 
          uMinDamgd = SVARS(49) 
          P_kunload = SVARS(50) 
          CnCycle = SVARS(51) 
          elasticStrainEnergy = SVARS(52) 
          SPR_F = SVARS(56)
          SPR_K = SVARS(57)     
      ELSE IF (I_SPR_NUM .EQ. 2) THEN
          envlpPosDamgdStress(1) = SVARS(58)
          envlpPosDamgdStress(2) = SVARS(59) 
          envlpPosDamgdStress(3) = SVARS(60) 
          envlpPosDamgdStress(4) = SVARS(61)
          envlpPosDamgdStress(5) = SVARS(62)
          envlpPosDamgdStress(6) = SVARS(63)
          envlpNegDamgdStress(1) = SVARS(64)
          envlpNegDamgdStress(2) = SVARS(65) 
          envlpNegDamgdStress(3) = SVARS(66) 
          envlpNegDamgdStress(4) = SVARS(67)
          envlpNegDamgdStress(5) = SVARS(68)
          envlpNegDamgdStress(6) = SVARS(69) 
          state3Strain(1) = SVARS(70) 
          state3Strain(2) = SVARS(71) 
          state3Strain(3) = SVARS(72) 
          state3Strain(4) = SVARS(73) 
          state3Stress(1) = SVARS(74) 
          state3Stress(2) = SVARS(75) 
          state3Stress(3) = SVARS(76) 
          state3Stress(4) = SVARS(77) 
          state4Strain(1) = SVARS(78) 
          state4Strain(2) = SVARS(79) 
          state4Strain(3) = SVARS(80) 
          state4Strain(4) = SVARS(81) 
          state4Stress(1) = SVARS(82)
          state4Stress(2) = SVARS(83) 
          state4Stress(3) = SVARS(84) 
          state4Stress(4) = SVARS(85) 
          I_Cstate = INT(SVARS(86)) 
          Cstrain = SVARS(87) 
          Cstress = SVARS(88) 
          CstrainRate = SVARS(89) 
          P_lowI_CstateStrain = SVARS(90) 
          P_lowI_CstateStress = SVARS(91) 
          hghI_CstateStrain = SVARS(92) 
          hghI_CstateStress = SVARS(93) 
          CminStrainDmnd = SVARS(94) 
          CmaxStrainDmnd = SVARS(95) 
          Cenergy = SVARS(96) 
          CgammaK = SVARS(97) 
          CgammaD = SVARS(98) 
          CgammaF = SVARS(99) 
          gammaKUsed = SVARS(100) 
          gammaFUsed = SVARS(101) 
          Ttangent = SVARS(102) 
          P_kElasticPosDamgd = SVARS(103) 
          P_kElasticNegDamgd = SVARS(104) 
          uMaxDamgd = SVARS(105) 
          uMinDamgd = SVARS(106) 
          P_kunload = SVARS(107) 
          CnCycle = SVARS(108) 
          elasticStrainEnergy = SVARS(109) 
          SPR_F = SVARS(113)
          SPR_K = SVARS(114)     
      END IF
C
C
      RETURN
      END
C
C
C     ***********************************************************************************************************