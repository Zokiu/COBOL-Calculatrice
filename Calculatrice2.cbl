       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calcul2.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01  WS-TOTAL     PIC     9(20)v9(7)    VALUE ZERO.
       01  WS-TOTAL-ED  PIC     Z(20).99      VALUE ZERO.

       01  WS-NUMB1     PIC     9(03)         VALUE ZERO.
       01  WS-NUMB2     PIC     9(03)         VALUE ZERO.
       
       01  WS-MAINMENU  PIC     X(10)         VALUE SPACE.
       01  WS-ENCORE    PIC     X(04)         VALUE SPACE.

       PROCEDURE DIVISION.
       
           

           