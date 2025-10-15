*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001_EXIT INPUT.
  CASE TL_1001_01.
    WHEN TL_1002.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1001_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1001 OUTPUT.

  DATA: WA_FCODE1001 TYPE SY-UCOMM,
        IT_FCODE1001 LIKE TABLE OF WA_FCODE1001.

  IF TL_1001_01 IS INITIAL.
    TL_1001_01 = TL_1002.
    PERFORM ATUALIZA_ALV_ESTRUTURAS.
  ENDIF.

  CASE TL_1001_01.
    WHEN TL_1002.
      WA_FCODE1001 = 'REMOVER'.
      APPEND WA_FCODE1001 TO IT_FCODE1001.
      SET PF-STATUS 'PF1002' EXCLUDING IT_FCODE1001.
      SET TITLEBAR 'TL1002'.
    WHEN TL_1012.

      CLEAR: IT_FCODE1001.

      IF IT_ZGLT046_ALV-VERSNT NE '2' AND IT_ZGLT046_ALV-VERSNT NE '3'.
        WA_FCODE1001 = OK_VERIFICAR.
        APPEND WA_FCODE1001 TO IT_FCODE1001.
        WA_FCODE1001 = OK_DESATIVAR.
        APPEND WA_FCODE1001 TO IT_FCODE1001.
      ELSE.
        CASE CK_ANALISA_OBJETOS.
          WHEN ABAP_TRUE.
            WA_FCODE1001 = OK_VERIFICAR.
            APPEND WA_FCODE1001 TO IT_FCODE1001.
          WHEN ABAP_FALSE.
            WA_FCODE1001 = OK_DESATIVAR.
            APPEND WA_FCODE1001 TO IT_FCODE1001.
        ENDCASE.
      ENDIF.

      SET PF-STATUS 'PF1012' EXCLUDING IT_FCODE1001.
      SET TITLEBAR 'TL1012' WITH IT_ZGLT046_ALV-VERSN IT_ZGLT046_ALV-VSTXT.
  ENDCASE.


ENDMODULE.                 " STATUS_1001  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001 INPUT.

  IF TL_1001_01 EQ TL_1002.
    CASE OK_CODE_1001.
      WHEN OK_ATUALIZA.
        PERFORM ATUALIZA_ALV_ESTRUTURAS.
      WHEN OK_NOVA_EST.
        PERFORM NOVA_ESTRUTURA_BALANCO.
      WHEN OK_NOVA_PLAN.
        PERFORM CARREGA_ESTRUTURA_ARQUIVO.
      WHEN OK_COPI_EST.

      WHEN OK_DELE_EST.
        PERFORM APAGAR_ESTRUTURA_BALANCO.
      WHEN OK_BACK.

      WHEN 'REMOVER'.
        PERFORM COPIAR_OBJ_CUSTO_P_NIVEIS.
    ENDCASE.
  ENDIF.

  IF TL_1001_01 EQ TL_1012.
    CASE OK_CODE_1001.
      WHEN OK_BACK.
        TL_1001_01 = TL_1002.
        LEAVE TO SCREEN 1001.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1001  INPUT

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2463   text
*----------------------------------------------------------------------*
FORM MOSTRA_TEXTO  USING  P_TEXTO.

  DATA: VMSG(50).

  MOVE P_TEXTO TO VMSG.

  CALL FUNCTION 'TH_REDISPATCH'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.

ENDFORM.                    " MOSTRA_TEXTO

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2463   text
*----------------------------------------------------------------------*
FORM MOSTRA_TEXTO_P  USING P_TEXTO
                           P_TOTAL   TYPE I
                           P_POSICAO TYPE I.

  DATA: VMSG(100),
        P_PERCENTAGE(20),
        P_PERCE TYPE I.

  MOVE P_TEXTO TO VMSG.

  IF P_TOTAL NE 0.
    P_PERCE = ( P_POSICAO * 100 ) / P_TOTAL.
  ENDIF.

  WRITE P_PERCE TO P_PERCENTAGE.

  CALL FUNCTION 'TH_REDISPATCH'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = P_PERCENTAGE
      TEXT       = VMSG.

ENDFORM.                    " MOSTRA_TEXTO

*&---------------------------------------------------------------------*
*&      Form  COPIAR_OBJ_CUSTO_P_NIVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM COPIAR_OBJ_CUSTO_P_NIVEIS .

  DATA: IT_XXX_49   TYPE TABLE OF ZGLT049  WITH HEADER LINE,
        IT_XXX_49_C TYPE TABLE OF ZGLT049C WITH HEADER LINE,
        IT_XXX_49_L TYPE TABLE OF ZGLT049L WITH HEADER LINE,
        IT_XXX_49_M TYPE TABLE OF ZGLT049M WITH HEADER LINE,
        WA_CN       TYPE ZGLT049CN,
        WA_LN       TYPE ZGLT049LN,
        WA_MN       TYPE ZGLT049MN.

  SELECT * INTO TABLE IT_XXX_49 FROM ZGLT049.
  SELECT * INTO TABLE IT_XXX_49_C FROM ZGLT049C.
  SELECT * INTO TABLE IT_XXX_49_L FROM ZGLT049L.
  SELECT * INTO TABLE IT_XXX_49_M FROM ZGLT049M.

  LOOP AT IT_XXX_49.

    LOOP AT IT_XXX_49_C WHERE COD_CLAS_BAL EQ IT_XXX_49-COD_CLAS_BAL
                          AND COD_CLAS_NOT EQ IT_XXX_49-COD_CLAS_NOT.
      CLEAR WA_CN.
      WA_CN-VERSN        = IT_XXX_49-VERSN.
      WA_CN-NIVEL        = IT_XXX_49-NIVEL.
      WA_CN-COD_CLAS_BAL = IT_XXX_49_C-COD_CLAS_BAL.
      WA_CN-COD_CLAS_NOT = IT_XXX_49_C-COD_CLAS_NOT.
      WA_CN-KOSAR        = IT_XXX_49_C-KOSAR.
      MODIFY ZGLT049CN FROM WA_CN.
    ENDLOOP.

    LOOP AT IT_XXX_49_L WHERE COD_CLAS_BAL EQ IT_XXX_49-COD_CLAS_BAL
                          AND COD_CLAS_NOT EQ IT_XXX_49-COD_CLAS_NOT.
      CLEAR WA_LN.
      WA_LN-VERSN        = IT_XXX_49-VERSN.
      WA_LN-NIVEL        = IT_XXX_49-NIVEL.
      WA_LN-COD_CLAS_BAL = IT_XXX_49_L-COD_CLAS_BAL.
      WA_LN-COD_CLAS_NOT = IT_XXX_49_L-COD_CLAS_NOT.
      WA_LN-KOKRS        = IT_XXX_49_L-KOKRS.
      WA_LN-PRCTR        = IT_XXX_49_L-PRCTR.
      MODIFY ZGLT049LN FROM WA_LN.
    ENDLOOP.

    LOOP AT IT_XXX_49_M WHERE COD_CLAS_BAL EQ IT_XXX_49-COD_CLAS_BAL
                          AND COD_CLAS_NOT EQ IT_XXX_49-COD_CLAS_NOT.
      CLEAR WA_MN.
      WA_MN-VERSN        = IT_XXX_49-VERSN.
      WA_MN-NIVEL        = IT_XXX_49-NIVEL.
      WA_MN-COD_CLAS_BAL = IT_XXX_49_M-COD_CLAS_BAL.
      WA_MN-COD_CLAS_NOT = IT_XXX_49_M-COD_CLAS_NOT.
      WA_MN-MATKL        = IT_XXX_49_M-MATKL.
      MODIFY ZGLT049MN FROM WA_MN.
    ENDLOOP.

  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " COPIAR_OBJ_CUSTO_P_NIVEIS
