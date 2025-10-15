"Name: \PR:RFFOBR_U\FO:STORE_ON_FILE\SE:BEGIN\EI
ENHANCEMENT 0 Z_F110_TROCA.
*
  DATA: STRUCT1 TYPE J_1BDMEXH1,
        STRUCT2 TYPE J_1BDMEXH2,
        STRUCT3 TYPE j_1bdmeaa, "detalhe bradesco
        STRUCT4 TYPE J_1BDMEAH, "header  bradesco
        STRUCT5 TYPE J_1BDMEXA,

        Vdigito type J_1BDMEXH1-H112,
        vqtde   type i,
        vconta(7),
        Vsoci   type J_1BDMEAH-H04,

        VDIGITO2(1).
  FIELD-SYMBOLS: <COMPONENT> TYPE ANY.

  clear vqtde .
  Do.
     ASSIGN COMPONENT SY-INDEX OF STRUCTURE DATEN TO <COMPONENT>.
     IF SY-SUBRC NE 0.
        vqtde = SY-INDEX.
        EXIT.
     ENDIF.
  ENDDO.

  if ( vqtde = 22 or vqtde = 29  ).
      MOVE-CORRESPONDING DATEN TO STRUCT1.
      MOVE-CORRESPONDING DATEN TO STRUCT2.
      MOVE-CORRESPONDING DATEN TO STRUCT5.
      ASSIGN COMPONENT 1 OF STRUCTURE daten TO <COMPONENT>.
      if REGUH-RZAWE = 'X'  and STRUCT5 EQ DATEN. "PIX
         MOVE-CORRESPONDING DATEN TO STRUCT5.
         DO.
            ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
            IF SY-SUBRC NE 0.
              EXIT. " quando acabar a estrutura
            ENDIF.
            IF sy-index = 8.
               <COMPONENT> = '009'.
            ENDIF.
            IF sy-index = 19.
               <COMPONENT>+8(2) = '01'.
            ENDIF.
          ENDDO.
      endif.


      if <COMPONENT> = '001' and vqtde = 22 and STRUCT1 EQ DATEN. "estrutura 1 BBRA
         MOVE-CORRESPONDING DATEN TO STRUCT1.
         ASSIGN COMPONENT 6 OF STRUCTURE daten TO <COMPONENT>.
         Vsoci = <COMPONENT> .
         EXPORT Vsoci  TO MEMORY ID 'EMPRESA_BANCO'.
      elseIF <COMPONENT> = '001' and  vqtde = 29 and STRUCT2 EQ DATEN.
        IMPORT Vsoci  from MEMORY ID 'EMPRESA_BANCO'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = Vsoci
          IMPORTING
            OUTPUT = Vsoci.
          if Vsoci+0(9) = '023771214'. "empresa 41
            MOVE-CORRESPONDING DATEN TO STRUCT2.
            DO.
              ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
              IF SY-SUBRC NE 0.
                EXIT. " quando acabar a estrutura
              ENDIF.
              IF sy-index = 6.
                  if REGUH-RZAWE = 'S'.
                   <COMPONENT> = '41'.
                 elseif REGUH-RZAWE = 'U'.
                   <COMPONENT> = '01'.
                 endif.
              ENDIF.
             ENDDO.
          endif.
          if '015143827_010962697' CS Vsoci+0(9)  . "empresa  0035 e 0038
            MOVE-CORRESPONDING DATEN TO STRUCT2.
            DO.
              ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
              IF SY-SUBRC NE 0.
                EXIT. " quando acabar a estrutura
              ENDIF.
              IF sy-index = 6.
                  if REGUH-RZAWE = 'S'.
                   <COMPONENT> = '03'.
                 elseif REGUH-RZAWE = 'U'.
                   <COMPONENT> = '01'.
                 endif.
              ENDIF.
             ENDDO.
          endif.
           if REGUH-RZAWE = 'X'. "PIX
            MOVE-CORRESPONDING DATEN TO STRUCT2.
            DO.
              ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
              IF SY-SUBRC NE 0.
                EXIT. " quando acabar a estrutura
              ENDIF.
              IF sy-index = 6.
                  <COMPONENT> = '45'.
              ENDIF.
             ENDDO.
          endif.
      ELSEIF <COMPONENT> = '341'.
          MOVE-CORRESPONDING DATEN TO STRUCT1.
          MOVE-CORRESPONDING DATEN TO STRUCT2.
          clear: Vdigito.
          IF STRUCT1 EQ DATEN.
             DO.
               ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
              IF SY-SUBRC NE 0.
                EXIT. " quando acabar a estrutura
              ENDIF.
              IF sy-index = 11.
                   Vdigito =  <COMPONENT>.
                   <COMPONENT> = ''.
              ELSEIF sy-index = 12.
                   <COMPONENT> = Vdigito.
              ENDIF.
             ENDDO.
          ELSEIF STRUCT2 EQ DATEN.
             DO.
              ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
              IF SY-SUBRC NE 0.
                EXIT. " quando acabar a estrutura
              ENDIF.
              IF sy-index = 6.
                  if REGUH-RZAWE = 'M'.
                    <COMPONENT> = '03'.
                  elseif REGUH-RZAWE = 'S'.
                    <COMPONENT> = '41'.
                  elseif REGUH-RZAWE = 'U'.
                    <COMPONENT> = '01'.
                  elseif REGUH-RZAWE = 'X'. "PIX
                    <COMPONENT> = '45'.
                  endif.
              ELSEIF sy-index = 7.
                   <COMPONENT> = '040'.
              ELSEIF sy-index = 12.
                   <COMPONENT> = ' '.
              ENDIF.
             ENDDO.
          ENDIF.
      ENDIF.
  ELSEIF vqtde = 20.
      MOVE-CORRESPONDING DATEN TO STRUCT4.
      if STRUCT4 EQ DATEN.
         ASSIGN COMPONENT 4 OF STRUCTURE daten TO <COMPONENT>.
         Vsoci = <COMPONENT> .
         EXPORT Vsoci  TO MEMORY ID 'EMPRESA_BANCO'.
         IF REGUH-UBNKL(3) = '237'.
            DO.
              ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
              IF SY-SUBRC NE 0.
                EXIT. " quando acabar a estrutura
              ENDIF.
              IF sy-index = 18 and Vsoci+0(9) = '084590892'.
                  <COMPONENT>+219 = '0067000'.
              ENDIF.
            ENDDO.
         ENDIF.
      endif.
  ELSEIF vqtde = 43.
    IMPORT Vsoci  from MEMORY ID 'EMPRESA_BANCO'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = Vsoci
        IMPORTING
          OUTPUT = Vsoci.
    "
    IF Vsoci+0(9) = '084590892'. "Hermasa 00010
      IF REGUH-UBNKL(3) = '237'.
          DO.
            ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
            IF SY-SUBRC NE 0.
              EXIT. " quando acabar a estrutura
            ENDIF.
            IF sy-index = 41.
                <COMPONENT>+64 = '0067000'.
            ENDIF.
          ENDDO.
      ENDIF.
    ENDIF.
    "
    if not ( '015143827_010962697' CS Vsoci+0(9) ) . "empresa  0035 e 0038 não marca
      IF REGUH-UBNKL(3) = '237'.
         MOVE-CORRESPONDING DATEN TO STRUCT3.
         IF STRUCT3 EQ DATEN.
              DO.
                ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
               IF SY-SUBRC NE 0.
                 EXIT. " quando acabar a estrutura
               ENDIF.
               IF sy-index = 34.
                   <COMPONENT> = 25.
               ENDIF.
              ENDDO.
         endif.
      endif.
    ELSEIF '015143827_010962697' CS Vsoci+0(9)  . "empresa  0035 e 0038
         IF REGUH-UBNKL(3) = '237'.
         MOVE-CORRESPONDING DATEN TO STRUCT3.
         IF STRUCT3 EQ DATEN.
              DO.
                ASSIGN COMPONENT SY-INDEX OF STRUCTURE daten TO <COMPONENT>.
               IF SY-SUBRC NE 0.
                 EXIT. " quando acabar a estrutura
               ENDIF.
               IF sy-index = 41. "
                 SPLIT REGUH-UBKNT AT '-'
                      INTO VCONTA
                           VDIGITO2.
                 CONDENSE VCONTA NO-GAPS.

*                 CONCATENATE VCONTA VDIGITO2 INTO Vconta.

                 CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      INPUT  = vconta
                    IMPORTING
                      OUTPUT = vconta.
                   <COMPONENT>+63(1) = ''.
                   <COMPONENT>+64(7) = vconta.
               ELSEIF sy-index = 28.
                  IF ( REGUH-RZAWE EQ 'M' ).
                    <COMPONENT> = '03'.
                  ELSEIF ( REGUH-RZAWE EQ 'U' ).
                    <COMPONENT> = '05'.
                  ELSEIF ( 'ST' CS REGUH-RZAWE ).
                    <COMPONENT> = '08'.
                  ELSEIF ( REGUH-RZAWE EQ 'E' ).
                    <COMPONENT> = '31'.
                  ELSE.
                    <COMPONENT> = '00'.
                  ENDIF.
               ENDIF.

              ENDDO.
         endif.
      endif.
    endif.

  ENDIF.
ENDENHANCEMENT.
