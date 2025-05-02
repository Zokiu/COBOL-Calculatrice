       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calcul2.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *Variable pour effectuer les calculs.
       01  WS-TOTAL     PIC     S9(20)v9(03)    VALUE ZERO.
       01  WS-NUMB1     PIC     S9(03)v9(03)    VALUE ZERO.
       01  WS-OPERATOR  PIC      X(01)          VALUE SPACE.
       01  WS-NUMB2     PIC     S9(03)v9(03)    VALUE ZERO.

      *Variable réponse (pourrait être un bool)
       01  WS-ENCORE    PIC      X(04)          VALUE SPACE.

      *Variable d'affichage.
       01  WS-TOTAL-ED  PIC     -Z(20).99       VALUE ZERO.
       01  WS-NUMB1-ED  PIC     -Z(20).99       VALUE ZERO.
       01  WS-NUMB2-ED  PIC     -Z(20).99       VALUE ZERO.

      *Variable pour afficher l'historique complet.
       01  WS-DETAIL    PIC      X(255).
       01  WS-HIST      PIC      X(04)          VALUE SPACE.

      *Variable créée pour indexer WS-DETAIL afin de pouvoir effacer.
       01  WS-CONTROL   PIC      99             VALUE ZERO.

       PROCEDURE DIVISION.
      *Première saisie avec les fonctions liées à l'historique.
           DISPLAY "Entrez le premier nombre".
           ACCEPT WS-NUMB1.
           MOVE   WS-NUMB1 TO WS-NUMB1-ED.
           STRING FUNCTION TRIM(WS-DETAIL) DELIMITED BY SIZE 
                  FUNCTION TRIM(WS-NUMB1-ED)  DELIMITED BY SIZE 
                  INTO WS-DETAIL
           END-STRING.
           
      *Lancement du programme.     
           PERFORM 0400-INPUT-START
           THRU    0400-INPUT-END.
           
           STOP RUN.
           
      ******************************************************************

       0100-CALCUL-START.
      *Action du calcul en fonction de l'opérateur choisi.
           EVALUATE WS-OPERATOR        
                WHEN = "+"
                     ADD      WS-NUMB1 TO   WS-NUMB2 GIVING WS-TOTAL
                WHEN = "-"
                     SUBTRACT WS-NUMB2 FROM WS-NUMB1 GIVING WS-TOTAL
                WHEN = "*"
                     MULTIPLY WS-NUMB1 BY   WS-NUMB2 GIVING WS-TOTAL
                WHEN = "/"
                     DIVIDE   WS-NUMB1 INTO WS-NUMB2 GIVING WS-TOTAL
                WHEN = "^"
                     COMPUTE  WS-TOTAL = WS-NUMB1 ** WS-NUMB2
                WHEN OTHER
      *Envoi vers un paragraphe permettant la resaisie de l'opérateur.
                     DISPLAY "Opérateur non pris en compte"
                     PERFORM 0200-WRONG-OPERATOR-START
                     THRU    0200-WRONG-OPERATOR-END
              END-EVALUATE.

           EXIT.
       0100-CALCUL-END.

       0200-WRONG-OPERATOR-START.
      *Suppression de l'ancienne saisie erronée dans l'historique.
           INSPECT WS-DETAIL TALLYING WS-CONTROL
               FOR ALL CHARACTERS BEFORE INITIAL SPACES.
           MOVE " " TO WS-DETAIL(WS-CONTROL:1).
      *Nouvelle saisie de l'opérateur avec fonction liées à l'historique.
           DISPLAY "Insérez un opérateur correct (+, -, *, /, ^)".
           ACCEPT WS-OPERATOR.
           STRING FUNCTION TRIM(WS-DETAIL)   DELIMITED BY SIZE 
                                WS-OPERATOR  DELIMITED BY SIZE 
                  INTO WS-DETAIL
           END-STRING.
      *Retour au paragraphe de calcul.
           PERFORM 0100-CALCUL-START
           THRU    0100-CALCUL-END.

           EXIT.
       0200-WRONG-OPERATOR-END.

       0300-ENCORE-START.
      *Permet à l'utilisateur de continuer son calcul en gardant son résultat antérieur.
           DISPLAY "Voulez-vous continuer ? (YES, NO)".
           ACCEPT WS-ENCORE.
           EVALUATE WS-ENCORE
              WHEN = "YES"
      *On récupère le total pour continuer avec celui-ci.
                MOVE WS-TOTAL TO WS-NUMB1
      *On retourne au paragraphe de saisie initial pour finir la boucle.
                PERFORM 0400-INPUT-START
                THRU    0400-INPUT-END
              WHEN = "NO"
      *Envoie au pararaphe qui prépare et affiche le résultat final.
                DISPLAY "Le résultat de l'opération finale est : "
                PERFORM 0600-SHOW-RESULT-START
                THRU    0600-SHOW-RESULT-END
      *Permet à l'utilisateur de voir l'historique.
            DISPLAY "Voulez vous voir l'historique complet ? (YES/NO)"
            ACCEPT WS-HIST
            IF WS-HIST = "YES"
               THEN 
      *Prépare et transfert le résultat final dans l'historique.
                  MOVE WS-TOTAL TO WS-TOTAL-ED
                  STRING 
                  FUNCTION TRIM(WS-DETAIL)    DELIMITED BY SIZE
                                  "="         DELIMITED BY SIZE
                  FUNCTION TRIM(WS-TOTAL-ED)  DELIMITED BY SIZE
                  INTO WS-DETAIL
                  END-STRING
               DISPLAY WS-DETAIL
            END-IF
                STOP RUN
           WHEN OTHER
      *Envoie au paragraphe pour message d'erreur + relance.
                  PERFORM 0500-WRONG-ENCORE-START
                  THRU    0500-WRONG-ENCORE-END

           EXIT.
       0300-ENCORE-END.

       0400-INPUT-START.
      *Saisie initiale de l'opérateur avec fonction liée à l'historique.
           DISPLAY "Entrez l'opérateur (+, -, *, /, ^)".
           ACCEPT WS-OPERATOR.
           STRING FUNCTION TRIM(WS-DETAIL)   DELIMITED BY SIZE 
                                WS-OPERATOR  DELIMITED BY SIZE 
                  INTO WS-DETAIL
           END-STRING.
      *Saisie du 2ème nombre.
           DISPLAY "Entrez le deuxième nombre".    
           ACCEPT WS-NUMB2.
           MOVE   WS-NUMB2 TO WS-NUMB2-ED.
      *Envoie au paragraphe de calcul.
           PERFORM 0100-CALCUL-START
           THRU    0100-CALCUL-END.
      *Fonction liée à l'historique pour le 2ème nombre.
           STRING FUNCTION TRIM(WS-DETAIL) DELIMITED BY SIZE 
                  FUNCTION TRIM(WS-NUMB2-ED)  DELIMITED BY SIZE 
                  INTO WS-DETAIL
           END-STRING.
      *Envoie au paragraphe pour afficher le résultat.
           PERFORM 0600-SHOW-RESULT-START
           THRU    0600-SHOW-RESULT-END.
      *Envoie au paragraphe qui permet de boucler.
           PERFORM 0300-ENCORE-START
           THRU    0300-ENCORE-END.
           
           EXIT.
       0400-INPUT-END.

       0500-WRONG-ENCORE-START.
      *Paragraphe pour relancer suite à erreur de saisie.
           DISPLAY "Saisie incorrecte,"
           PERFORM 0300-ENCORE-START
           THRU    0300-ENCORE-END.
           
           EXIT.
       0500-WRONG-ENCORE-END.

       0600-SHOW-RESULT-START.
      *Prépare et affiche le résultat.
           MOVE WS-NUMB1 TO WS-NUMB1-ED.
           MOVE WS-NUMB2 TO WS-NUMB2-ED.
           MOVE WS-TOTAL TO WS-TOTAL-ED.
           DISPLAY FUNCTION TRIM(WS-NUMB1-ED) WS-OPERATOR
                   FUNCTION TRIM(WS-NUMB2-ED) "=" 
                   FUNCTION TRIM(WS-TOTAL-ED).

           EXIT.
       0600-SHOW-RESULT-END.


