       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calcul2.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01  WS-TOTAL     PIC     S9(20)v9(03)    VALUE ZERO.
       01  WS-NUMB1     PIC     S9(03)v9(03)    VALUE ZERO.
       01  WS-OPERATOR  PIC      X(01)          VALUE SPACE.
       01  WS-NUMB2     PIC     S9(03)v9(03)    VALUE ZERO.

       01  WS-ENCORE    PIC      X(04)          VALUE SPACE.

       01  WS-TOTAL-ED  PIC     -Z(20).99       VALUE ZERO.
       01  WS-NUMB1-ED  PIC     -Z(20).99       VALUE ZERO.
       01  WS-NUMB2-ED  PIC     -Z(20).99       VALUE ZERO.

       PROCEDURE DIVISION.

           DISPLAY "Entrez le premier nombre".
           ACCEPT WS-NUMB1.
           
           PERFORM 0400-INPUT-START
           THRU    0400-INPUT-END.
           
           STOP RUN.
           
      ******************************************************************

       0100-CALCUL-START.
           EVALUATE WS-OPERATOR        
                WHEN = "+"
                     ADD      WS-NUMB1 TO   WS-NUMB2 GIVING WS-TOTAL
                WHEN = "-"
                     SUBTRACT WS-NUMB1 FROM WS-NUMB2 GIVING WS-TOTAL
                WHEN = "*"
                     MULTIPLY WS-NUMB1 BY   WS-NUMB2 GIVING WS-TOTAL
                WHEN = "/"
                     DIVIDE   WS-NUMB1 INTO WS-NUMB2 GIVING WS-TOTAL
                WHEN = "^"
                     COMPUTE  WS-TOTAL = WS-NUMB1 ** WS-NUMB2
                WHEN OTHER
                     DISPLAY "Opérateur non pris en compte"
                     PERFORM 0200-WRONG-OPERATOR-START
                     THRU    0200-WRONG-OPERATOR-END
              END-EVALUATE.

           EXIT.
       0100-CALCUL-END.

       0200-WRONG-OPERATOR-START.
           DISPLAY "Insérez un opérateur correct (+, -, *, /, ^)".
           ACCEPT WS-OPERATOR.
           PERFORM 0100-CALCUL-START
           THRU    0100-CALCUL-END.

           EXIT.
       0200-WRONG-OPERATOR-END.

       0300-ENCORE-START.
           DISPLAY "Voulez-vous continuer ? (YES, NO)".
           ACCEPT WS-ENCORE.
           EVALUATE WS-ENCORE
              WHEN = "YES"
                MOVE WS-TOTAL TO WS-NUMB1
                PERFORM 0400-INPUT-START
                THRU    0400-INPUT-END
              WHEN = "NO"
                DISPLAY "Le résultat de l'opération finale est : "
                PERFORM 0600-SHOW-RESULT-START
                THRU    0600-SHOW-RESULT-END
                STOP RUN
              WHEN OTHER
                     PERFORM 0500-WRONG-ENCORE-START
                     THRU    0500-WRONG-ENCORE-END

           EXIT.
       0300-ENCORE-END.

       0400-INPUT-START.
           DISPLAY "Entrez l'opérateur (+, -, *, /, ^)".
           ACCEPT WS-OPERATOR.
           DISPLAY "Entrez le deuxième nombre".    
           ACCEPT WS-NUMB2.
           PERFORM 0100-CALCUL-START
           THRU    0100-CALCUL-END.
           PERFORM 0600-SHOW-RESULT-START
           THRU    0600-SHOW-RESULT-END.
           PERFORM 0300-ENCORE-START
           THRU    0300-ENCORE-END.

           EXIT.
       0400-INPUT-END.

       0500-WRONG-ENCORE-START.
           DISPLAY "Saisie incorrecte,"
           PERFORM 0300-ENCORE-START
           THRU    0300-ENCORE-END.
           
           EXIT.
       0500-WRONG-ENCORE-END.

       0600-SHOW-RESULT-START.
           MOVE WS-NUMB1 TO WS-NUMB1-ED.
           MOVE WS-NUMB2 TO WS-NUMB2-ED.
           MOVE WS-TOTAL TO WS-TOTAL-ED.
           DISPLAY FUNCTION TRIM(WS-NUMB1-ED) WS-OPERATOR
                   FUNCTION TRIM(WS-NUMB2-ED) "=" 
                   FUNCTION TRIM(WS-TOTAL-ED).

           EXIT.
       0600-SHOW-RESULT-END.


