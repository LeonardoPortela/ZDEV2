*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_PAI_0160
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  PAI_0160  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0160 INPUT.
  CASE SY-UCOMM.
    WHEN 'BTN_EXECUTAR'.
      DATA: V_TEXTO_INFO TYPE CHAR20.
      CREATE OBJECT R_SELECIONA_DADOS.

      CASE SCREEN_ITEM.
        WHEN C_SCREEN_0170.

          IF ( S1_BUKRS-LOW IS INITIAL ).
            MESSAGE S836(SD) WITH TEXT-E03 DISPLAY LIKE 'E'.
          ELSE.

            R_SELECIONA_DADOS->SELECIONA_DADOS_0170( ).

            IF ( GT_SAIDA_0170 IS NOT INITIAL ).
              PERFORM HEADER_TITLE USING TEXT-I02.

              IF ( S1_BUKRS-HIGH IS NOT INITIAL ).
                CONCATENATE S1_BUKRS-LOW 'até' S1_BUKRS-HIGH INTO V_TEXTO_INFO SEPARATED BY SPACE.
              ELSE.
                V_TEXTO_INFO = S1_BUKRS-LOW.
              ENDIF.

              PERFORM HEADER_INFO USING 'Empresa:' V_TEXTO_INFO.

*            if ( s1_anln1 is not initial ).
*
*              if s1_anln1-high is not initial.
*                concatenate s1_anln1-low 'até' s1_anln1-high into v_texto_info separated by space.
*              else.
*                v_texto_info = s1_anln1-low.
*              endif.
*
*              perform header_info using 'Imobilizado:' v_texto_info.
*
*            endif.

              CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
                EXPORTING
                  I_CALLBACK_PROGRAM     = SY-REPID
                  I_CALLBACK_TOP_OF_PAGE = 'TOP-OF-PAGE'
                  I_DEFAULT              = 'X'
                  I_SAVE                 = 'X'
                  IS_LAYOUT              = WL_LAYOUT_SLIS
                  IT_FIELDCAT            = GT_FCAT_SLIS
                TABLES
                  T_OUTTAB               = GT_SAIDA_0170
                EXCEPTIONS
                  PROGRAM_ERROR          = 1
                  OTHERS                 = 2.

            ELSE.
              MESSAGE S836(SD) WITH TEXT-E06 DISPLAY LIKE 'E'.
            ENDIF.

          ENDIF.

        WHEN C_SCREEN_0180.

          IF ( S2_BUKRS-LOW IS INITIAL ).
            MESSAGE S836(SD) WITH TEXT-E03 DISPLAY LIKE 'E'.
          ELSE.
          R_SELECIONA_DADOS->SELECIONA_DADOS_0180( ).
*
          IF ( GT_SAIDA_0180 IS NOT INITIAL ).
            PERFORM HEADER_TITLE USING TEXT-I03.

            IF ( S2_BUKRS-HIGH IS NOT INITIAL ).
              CONCATENATE S2_BUKRS-LOW 'até' S2_BUKRS-HIGH INTO V_TEXTO_INFO SEPARATED BY SPACE.
            ELSE.
              V_TEXTO_INFO = S2_BUKRS-LOW.
            ENDIF.

            PERFORM HEADER_INFO USING 'Empresa:' V_TEXTO_INFO.
            PERFORM HEADER_INFO USING 'Tipo de Obrigação:' TP_OBRIGACAO.


            CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
              EXPORTING
                I_CALLBACK_PROGRAM     = SY-REPID
                I_CALLBACK_TOP_OF_PAGE = 'TOP-OF-PAGE'
                I_DEFAULT              = 'X'
                I_SAVE                 = 'X'
                IS_LAYOUT              = WL_LAYOUT_SLIS
                IT_FIELDCAT            = GT_FCAT_SLIS
              TABLES
                T_OUTTAB               = GT_SAIDA_0180
              EXCEPTIONS
                PROGRAM_ERROR          = 1
                OTHERS                 = 2.

          ELSE.
            MESSAGE S836(SD) WITH TEXT-E06 DISPLAY LIKE 'E'.
          ENDIF.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " PAI_0160  INPUT
