CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C       ***UNPACK.FOR***     (12-15-89)                                C
C                                                                      C
C                                                                      C
C          THIS FORTRAN PROGRAM TAKES COMPRESSED PRECISE EPHEMERIS     C
C       SATELLITE DATA (AT 80 CHARACTERS PER LINE) AND EXPANDS IT TO   C
C       102 CHARACTERS PER LINE, WHICH IS THE NORMAL FORMAT OF THE     C
C       PRECISE EPHEMERIS.                                             C
C          THIS PROGRAM IS USED TO RECONSTRUCT THE PRECISE EPHEMERIS   C
C       WHEN IT IS TRANSFERRED FROM THE MAINFRAME TO THE IBM PC.  THE  C
C       PRECISE EPHEMERIS ON THE MAINFRAME IS COMPRESSED BY TAKING ALL C
C       THE BLANK CHARACTERS OUT OF THE 102 CHARACTER PER LINE PRECISE C
C       EPHEMERIS FILE AND WRITING INTO A 80 CHARACTERS PER  LINE      C
C       FILE.  THIS HELPS IN TRANSFERRING THE FILE AS EFFIENCTLY  AS   C
C       POSSIBLE USING THE FILE TRANSFER PROGRAM KERMIT.  THIS PROGRAM C
C       THEN RESTORES THE PRECISE EPHEMERIS TO ITS CORRECT 102         C
C       CHARACTER PER LINE FORMAT.                                     C
C                                                                      C
C       JAMES McDONALD                                                 C
C       BYRD POLAR RESEARCH CENTER                                     C
C       OHIO STATE UNIVERSITY                                          C
C       COLUMBUS, OHIO  43210                                          C
C                                                         				  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C----------------------------------------------------------------------C
C      VARIABLE DEFINITIONS                                            C
C                                                                      C
C      AMP                CONSTANT THAT CONTAINS THE "AT" CHARACTER    C
C      BLANK              CONSTANT THAT CONTAINS A BLANK CHARACTER     C
C      BUFFER(J)          CHARACTER ARRAY THAT CONTAINS ONE LINE OF    C
C                         EXPANDED DATA, 102 CHARACTERS.               C
C      DECPT              CONSTANT THAT CONTAINS A DECIMAL POINT. THIS C 
C                         CONSTANT IS USED TO FIND THE DECIMAL POINTS  C
C                         OF THE SIX NUMBERS IN THE COMPRESSED DATA.   C
C      DEL                NUMBER OF CHARACTERS IN A NUMBER STRING      C
C      END                INDEX THAT MARKS THE END OF THE VARIABLE     C
C                         TO BE RECONSTRUCTED                          C
C      I                  INDEX FOR THE COMPRESSED DATA                C
C      IAMP               INDEX MARKER FOR THE LOCATION OF THE "AT"    C
C                         SYMBOL.                                      C
C      IDECPT             INDEX MARKER FOR THE LOCATION OF THE DECIMAL C
C                         POINT.                                       C
C      INPUT              INPUT BUFFER ARRAY, 80 CHARACTERS            C
C      J                  INDEX FOR THE EXPANDED OR NORMAL DATA        C
C      K                  INDEX THAT CONTAINS THE NUMBER OF CHARACTERS C
C                         THAT ARE A REMAINDER FROM THE COMPRESSED     C
C                         DATA.                                        C
C      NBLAMK             THE NUMBER OF BLANKS TO INSERT BETWEEN       C
C                         CHARACTER STRINGS                            C
C      NDECPT             NUMBER OF DECIMAL POINTS ENCOUNTERED DURING  C
C                         RECONSTRUCTION OF LINE OF EXPANDED OR NORMAL C
C                         PRECISE EPHEMERIS                            C
C      NSAT               LENGTH OF THE SATELLITE CHARACTER STRING     C
C      REMAIN             THE NUMBER OF CHARACTERS THAT REMAIN IN THE  C
C                         INPUT ARRAY                                  C
C      START              INDEX THAT MARKS THE BEGINNING OF THE        C
C                         VARIABLE TO BE RECONSTRUCTED                 C
C      TEMP               TEMPORARY CHARACTER ARRAY                    C
C----------------------------------------------------------------------C
      CHARACTER*1 INPUT(80),BUFFER(102),OUT(30),AMP,BLANK,DECPT,
     1            TEMP(80)
      CHARACTER*30 OUTPUT
      INTEGER END,START,DEL,REMAIN
      DATA AMP/'@'/,BLANK/' '/,DECPT/'.'/
      J=1
      NDECPT=0
      REMAIN=0
      NSAT=0
      START=1
      END=1
      OPEN(7,FILE='SAT.DAT',STATUS='OLD')
      WRITE(0,900)
900   FORMAT(' ','OUTPUT FILE NAME (UP TO 30 CHARACTERS)')
      READ(0,901) (OUT(I),I=1,30)
901   FORMAT(30A1)
      DO 1 I=1,30
1     OUTPUT(I:30)=OUT(I)
      OPEN(8,FILE=OUTPUT,STATUS='NEW')
C**********************************************************************C
C      START READING THE COMPRESSED FILE, LINE BY LINE                 C
C**********************************************************************C
2     READ(7,902,END=4) (INPUT(I),I=1,80)
902   FORMAT(80A1)
C**********************************************************************C
C      START THE RECONSTRUCTION                                        C
C**********************************************************************C
      DO 3 I=1,80
C**********************************************************************C
C      RECONSTRUCT THE TIME AND DATE (CHARACTERS 1-12)                 C
C**********************************************************************C
        IF(J.LE.12) THEN
          IF(REMAIN.NE.0.AND.I.EQ.1) THEN
             END=REMAIN
             DO 52 K=1,END
               IF(J.EQ.3.OR.J.EQ.7.OR.J.EQ.10) THEN
                 BUFFER(J)=BLANK
                 J=J+1
               ENDIF
               BUFFER(J)=INPUT(K)
               J=J+1
52           CONTINUE
             REMAIN=0
             START=END+1
          ELSEIF(REMAIN.EQ.0.AND.I.EQ.START) THEN
            END=START+8
            IF(END.GT.80) THEN
              REMAIN=END-80
              DO 50 K=START,80
                IF(J.EQ.3.OR.J.EQ.7.OR.J.EQ.10) THEN
                  BUFFER(J)=BLANK
                  J=J+1
                ENDIF
                BUFFER(J)=INPUT(K)
                J=J+1
50            CONTINUE
            ELSE
              DO 51 K=START,END
                IF(J.EQ.3.OR.J.EQ.7.OR.J.EQ.10) THEN
                  BUFFER(J)=BLANK
                  J=J+1
                ENDIF
                BUFFER(J)=INPUT(K)
                J=J+1
51            CONTINUE
              START=END+1
              IF(START.GT.80) START=START-80
            ENDIF
          ELSE
            CONTINUE
          ENDIF
C**********************************************************************C
C      RECONSTRUCT THE POSTION AND VELOCITY (CHARACTERS 13-90)         C
C      THESE INCLUDE THE THREE COMPONENTS OF THE POSITION AND THEIR    C
C      VELOCITY COMPONENTS.  THERE ARE A TOTAL OF 6 NUMBERS TO FIND.   C
C**********************************************************************C
        ELSEIF(J.GT.12.AND.J.LE.90) THEN
          IF(REMAIN.NE.0.AND.I.EQ.1) THEN
            DO 17 K=1,REMAIN
              BUFFER(J)=INPUT(K)
              J=J+1
17          CONTINUE
            START=REMAIN+1
            REMAIN=0
          ELSE
            IF(INPUT(I).EQ.DECPT) THEN
              NDECPT=NDECPT+1
              IDECPT=I
              IF(NDECPT.LE.3) GOTO 6
              IF(NDECPT.GT.3) GOTO 7
6               END=I+6
              GOTO 8
7               END=I+9
8             CONTINUE
              IF(START.GT.IDECPT) THEN
	        REMAIN=80-START+1
                DEL=REMAIN+END
                NBLANK=13-DEL
                DO 20 K=1,NBLANK
                  BUFFER(J)=BLANK
                  J=J+1
20              CONTINUE
                DO 21 K=START,80
                  BUFFER(J)=TEMP(K)
                  J=J+1
21              CONTINUE
                DO 60 K=1,END
                  BUFFER(J)=INPUT(K)
                  J=J+1
60              CONTINUE
                START=END+1
                IF(START.GT.80) START=START-80
                REMAIN=0
              ELSEIF(END.GT.80) THEN
                REMAIN=END-80
                DEL=END-START+1
                NBLANK=13-DEL
                DO 22 K=1,NBLANK
                  BUFFER(J)=BLANK
                  J=J+1
22              CONTINUE
                DO 23 K=START,80
                  BUFFER(J)=INPUT(K)
                  J=J+1
23              CONTINUE
                START=END+1-80
              ELSE
                DEL=END-START+1
                NBLANK=13-DEL
                DO 9 K=1,NBLANK
                  BUFFER(J)=BLANK
                  J=J+1
9               CONTINUE
                DO 10 K=START,END
                  BUFFER(J)=INPUT(K)
                  J=J+1
10              CONTINUE
                START=END+1
                IF(START.GT.80) START=START-80      
              ENDIF
            ELSEIF(I.EQ.80) THEN
              DO 11 K=START,80
                TEMP(K)=INPUT(K)
11            CONTINUE
            ELSE
              CONTINUE
            ENDIF
          ENDIF
          IF(NDECPT.EQ.6) NDECPT=0
C**********************************************************************C
C      FIND THE SATELLITE NUMBER (CHARACTERS 91-102)                   C
C**********************************************************************C
        ELSE
          IF(REMAIN.NE.0.AND.I.EQ.1) THEN
            DO 37 K=1,REMAIN
              BUFFER(J)=INPUT(K)
              J=J+1
37          CONTINUE
            START=REMAIN+1
            REMAIN=0
          ELSEIF(REMAIN.EQ.0) THEN
            IF(INPUT(I).EQ.AMP) THEN
              IAMP=I
              IF(START.GT.IAMP) THEN
                NSAT=IAMP-START+80
                GOTO (41,42),NSAT
41                NBLANK=7
                  GOTO 43
42                NBLANK=6
43              CONTINUE
                END=IAMP+3
                DO 44 K=1,NBLANK
                  BUFFER(J)=BLANK
                  J=J+1
44              CONTINUE
                DO 45 K=START,80
                  BUFFER(J)=TEMP(K)
                  J=J+1
45              CONTINUE
                DO 46 K=1,END
                  BUFFER(J)=INPUT(K)
                  J=J+1
46              CONTINUE
                START=END+1
                IF(START.GT.80) START=START-80
                NSAT=0
              ELSE
                NSAT=IAMP-START
                GOTO (31,32),NSAT
31                NBLANK=7
                  GOTO 33
32                NBLANK=6
33              CONTINUE
                END=IAMP+3
                IF(END.GT.80) REMAIN=END-80
                DO 34 K=1,NBLANK
                  BUFFER(J)=BLANK
                  J=J+1
34              CONTINUE
                DO 35 K=START,END-REMAIN
                  BUFFER(J)=INPUT(K)
                  J=J+1
35              CONTINUE
                START=END+1
                IF(START.GT.80) START=START-80
                NSAT=0
              ENDIF
            ELSEIF(I.EQ.80) THEN
              DO 36 K=START,80
                TEMP(K)=INPUT(K)
36            CONTINUE
            ELSE
              CONTINUE
            ENDIF
          ELSE
          CONTINUE
          ENDIF
        ENDIF
C------END OF RECONSTRUCTION OF THE LINE.  WRITE THE LINE TO THE FILE--C
        IF(J.EQ.103) THEN
          WRITE(8,903) (BUFFER(J),J=1,102)
903       FORMAT(102A1)
          J=1
        ENDIF
3     CONTINUE
C**********************END OF DO LOOP  I=1,80**************************C
      GOTO 2
4     CONTINUE
C      WRITE(8,903) (BUFFER(J),J=1,102)
      STOP
      END
