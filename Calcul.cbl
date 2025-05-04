       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calcul.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-TOTAL     PIC     9(20)v9(7)    VALUE ZERO.
       01  WS-TOTAL-ED  PIC     Z(20).99      VALUE ZERO.

       01  WS-NUMB1     PIC     9(03)         VALUE ZERO.
       01  WS-NUMB2     PIC     9(03)         VALUE ZERO.
      * 01  WS-RESULT    PIC     9(20)         VALUE ZERO.
       
       01  WS-MAINMENU  PIC     X(10)         VALUE SPACE.
       01  WS-ENCORE    PIC     X(04)         VALUE SPACE.

       PROCEDURE DIVISION.

           DISPLAY "Bienvenue dans votre Calculatrice"

           PERFORM UNTIL WS-MAINMENU = "QUIT"
               DISPLAY "Veuillez choisir le type d'opération"
               DISPLAY "ADD"
               DISPLAY "SUBTRACT"
               DISPLAY "MULTIPLY"
               DISPLAY "DIVIDE"
               DISPLAY "RESET"
               DISPLAY "QUIT"
               ACCEPT   WS-MAINMENU
               EVALUATE WS-MAINMENU
                   WHEN = "ADD"
                      PERFORM 0100-ADD-START
                      THRU    0100-ADD-END
                   WHEN = "SUBTRACT"
                      PERFORM 0200-SUBTRACT-START
                      THRU    0200-SUBTRACT-END
                   WHEN = "MULTIPLY"
                      PERFORM 0300-MULTIPLY-START
                      THRU    0300-MULTIPLY-END
                   WHEN = "DIVIDE"
                      PERFORM 0400-DIVIDE-START
                      THRU    0400-DIVIDE-END
                   WHEN = "RESET"
                      PERFORM 0500-RESET-START
                      THRU    0500-RESET-END
                   WHEN = "QUIT"
                       STOP RUN
                   WHEN OTHER
                      DISPLAY "Saisie erronée, veuillez recommencer"
                      CONTINUE
               END-EVALUATE
           END-PERFORM.

           STOP RUN.
       
      ******************************************************************
       
       0100-ADD-START.
           DISPLAY "Vous avez choisi les additions".
           DISPLAY "Veuillez choisir votre premier nombre".
           ACCEPT WS-NUMB1.
           DISPLAY "Veuillez choisir votre deuxième nombre".
           ACCEPT WS-NUMB2.
           ADD WS-NUMB1 TO WS-NUMB2 GIVING WS-TOTAL.
           DISPLAY WS-NUMB1 " + " WS-NUMB2.
           DISPLAY "Le résultat est : ".
           MOVE    WS-TOTAL TO WS-TOTAL-ED.
           DISPLAY FUNCTION TRIM(WS-TOTAL-ED).
           PERFORM UNTIL WS-ENCORE = "NO"
               DISPLAY "Voulez-vous continuer ? YES/NO"
               ACCEPT WS-ENCORE
               EVALUATE WS-ENCORE
                   WHEN = "YES"
                       DISPLAY "Veuillez choisir la nombre à ajouter"
                       ACCEPT WS-NUMB1
                       ADD WS-TOTAL TO WS-NUMB1 GIVING WS-TOTAL
                       DISPLAY WS-TOTAL " + " WS-NUMB1
                       DISPLAY "Le résultat est : "
                       MOVE    WS-TOTAL TO WS-TOTAL-ED
                       DISPLAY FUNCTION TRIM(WS-TOTAL-ED)
                   WHEN = "NO"
                       DISPLAY "Vous quittez les additions"
                   WHEN OTHER
                       DISPLAY "Saisie erronée, veuillez recommencer"
                       CONTINUE
                END-EVALUATE
           END-PERFORM.
           MOVE SPACES TO WS-ENCORE.
           EXIT.
       0100-ADD-END.

       0200-SUBTRACT-START.
           DISPLAY "Vous avez choisi les soustractions".
           DISPLAY "Veuillez choisir votre premier nombre".
           ACCEPT WS-NUMB1.
           DISPLAY "Veuillez choisir votre deuxième nombre".
           ACCEPT WS-NUMB2.
           SUBTRACT WS-NUMB1 FROM WS-NUMB2 GIVING WS-TOTAL.
           DISPLAY WS-NUMB1 " - " WS-NUMB2.
           DISPLAY "Le résultat est : ".
           MOVE    WS-TOTAL TO WS-TOTAL-ED.
           DISPLAY FUNCTION TRIM(WS-TOTAL-ED).
           PERFORM UNTIL WS-ENCORE = "NO"
               DISPLAY "Voulez-vous continuer ? YES/NO"
               ACCEPT WS-ENCORE
               EVALUATE WS-ENCORE
                   WHEN = "YES"
                     DISPLAY "Veuillez choisir le nombre à soustraire"
                     ACCEPT WS-NUMB1
                     SUBTRACT WS-NUMB1 FROM WS-TOTAL GIVING WS-TOTAL
                     DISPLAY WS-TOTAL " - " WS-NUMB1
                     DISPLAY "Le résultat est : "
                     MOVE    WS-TOTAL TO WS-TOTAL-ED
                     DISPLAY FUNCTION TRIM(WS-TOTAL-ED)
                   WHEN = "NO"
                     DISPLAY "Vous quittez les soustractions"
                   WHEN OTHER
                     DISPLAY "Saisie erronée, veuillez recommencer"
                       CONTINUE
                END-EVALUATE
           END-PERFORM.
           MOVE SPACES TO WS-ENCORE.
           EXIT.
       0200-SUBTRACT-END.

       0300-MULTIPLY-START.
           DISPLAY "Vous avez choisi les multiplications".
           DISPLAY "Veuillez choisir votre premier nombre".
           ACCEPT WS-NUMB1.
           DISPLAY "Veuillez choisir votre deuxième nombre".
           ACCEPT WS-NUMB2.
           MULTIPLY WS-NUMB1 BY WS-NUMB2 GIVING WS-TOTAL.
           DISPLAY WS-NUMB1 " x " WS-NUMB2.
           DISPLAY "Le résultat est : ".
           MOVE    WS-TOTAL TO WS-TOTAL-ED.
           DISPLAY FUNCTION TRIM(WS-TOTAL-ED).
           PERFORM UNTIL WS-ENCORE = "NO"
               DISPLAY "Voulez-vous continuer ? YES/NO"
               ACCEPT WS-ENCORE
               EVALUATE WS-ENCORE
                   WHEN = "YES"
                     DISPLAY "Veuillez choisir par combien multiplier"
                     ACCEPT WS-NUMB1
                     MULTIPLY WS-NUMB1 BY WS-TOTAL GIVING WS-TOTAL
                     DISPLAY WS-TOTAL " x " WS-NUMB1
                     DISPLAY "Le résultat est : "
                     MOVE    WS-TOTAL TO WS-TOTAL-ED
                     DISPLAY FUNCTION TRIM(WS-TOTAL-ED)
                   WHEN = "NO"
                     DISPLAY "Vous quittez les multiplications"
                   WHEN OTHER
                     DISPLAY "Saisie erronée, veuillez recommencer"
                       CONTINUE
                END-EVALUATE
           END-PERFORM.
           MOVE SPACES TO WS-ENCORE.
           EXIT.
       0300-MULTIPLY-END.

       0400-DIVIDE-START.
           DISPLAY "Vous avez choisi les divisions".
           DISPLAY "Veuillez choisir votre premier nombre".
           ACCEPT WS-NUMB1.
           DISPLAY "Veuillez choisir votre deuxième nombre".
           ACCEPT WS-NUMB2.
           DIVIDE WS-NUMB1 BY WS-NUMB2 GIVING WS-TOTAL.
           DISPLAY WS-NUMB1 " / " WS-NUMB2.
           DISPLAY "Le résultat est : ".
           MOVE    WS-TOTAL TO WS-TOTAL-ED.
           DISPLAY FUNCTION TRIM(WS-TOTAL-ED).
           PERFORM UNTIL WS-ENCORE = "NO"
               DISPLAY "Voulez-vous continuer ? YES/NO"
               ACCEPT WS-ENCORE
               EVALUATE WS-ENCORE
                   WHEN = "YES"
                       DISPLAY "Veuillez choisir par combien diviser"
                       ACCEPT WS-NUMB1
                       DIVIDE WS-TOTAL BY WS-NUMB1 GIVING WS-TOTAL
                       DISPLAY WS-TOTAL " / " WS-NUMB1
                       DISPLAY "Le résultat est : "
                       MOVE    WS-TOTAL TO WS-TOTAL-ED
                       DISPLAY FUNCTION TRIM(WS-TOTAL-ED)
                   WHEN = "NO"
                       DISPLAY "Vous quittez les divisions"
                   WHEN OTHER
                       DISPLAY "Saisie erronée, veuillez recommencer"
                       CONTINUE
                END-EVALUATE
           END-PERFORM.
           MOVE SPACES TO WS-ENCORE.
           EXIT.
       0400-DIVIDE-END.

       0500-RESET-START.
           MOVE 0      TO WS-TOTAL.
           MOVE 0      TO WS-TOTAL-ED.
           EXIT.
       0500-RESET-END.
       