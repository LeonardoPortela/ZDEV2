*&---------------------------------------------------------------------*
*&  Include           ZLESR0109_PAI
*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.

      DATA(_VIS_ATUAL) = ''.

      IF P_G_ADT IS NOT INITIAL.
        _VIS_ATUAL  = 'G'.
      ELSEIF P_C_ADT IS NOT INITIAL.
        _VIS_ATUAL  = 'C'.
      ENDIF.

      IF ( OBJ_ALV_0100 IS NOT INITIAL ) AND ( VG_VIS_ATUAL NE _VIS_ATUAL ).
        VG_VIS_ATUAL = _VIS_ATUAL.
        CALL METHOD OBJ_ALV_0100->FREE.
        CALL METHOD CL_GUI_CFW=>FLUSH.
        FREE: OBJ_ALV_0100.
      ENDIF.

      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.

  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0101 INPUT.

  DATA: IT_RSPARAMS TYPE TABLE OF RSPARAMS,
        WA_RSPARAMS TYPE RSPARAMS.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      IF P_BUKRS-LOW IS INITIAL.
        MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF P_LIFNR-LOW IS INITIAL.
        MESSAGE 'Fornecedor é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF VG_DATA_PROC IS INITIAL.
        MESSAGE 'Informe uma data para processamento!' TYPE 'S'.
        EXIT.
      ENDIF.

*       SELECT SINGLE *
*         FROM ZLEST0141 INTO @DATA(WL_0141)
*        WHERE BUKRS = @P_BUKRS-LOW
*          AND DATA  = @VG_DATA_PROC.
*
*       IF ( SY-SUBRC EQ 0 ) AND ( WL_0141-VLR_MOV IS INITIAL ).
*         MESSAGE 'Já existe movimento calculado para esta data!' TYPE 'S'.
*         EXIT.
*       ENDIF.

      SUBMIT ZLESR0108 WITH P_DATA   = VG_DATA_PROC
                       WITH P_PROC   = 'X'
         AND RETURN.

      MESSAGE 'Processamento concluído com sucesso!' TYPE 'S'.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

FORM F_MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                              VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                              VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                              VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                              VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                              VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                              VALUE(P_OUTPUTLEN)
                              VALUE(P_EDIT)
                              VALUE(P_DO_SUM).

  CLEAR: WA_ESTRUTURA.

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  WA_ESTRUTURA-DO_SUM        = P_DO_SUM.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  WA_ESTRUTURA-OUTPUTLEN = P_OUTPUTLEN.

  IF WA_ESTRUTURA-FIELDNAME EQ 'TYPE_IC'.
    WA_ESTRUTURA-JUST = 'C'.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

MODULE CANCEL_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANCEL' OR 'BACK'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102 INPUT.
  DATA W_ZLEST0195 TYPE ZLEST0195.
  CLEAR W_ZLEST0195.
  CASE SY-UCOMM.
    WHEN 'CONFIRM'.
      IF WA_ZLEST0195-DT_PGTO IS INITIAL.
        MESSAGE 'Informe uma data de vencimento!' TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT * FROM ZLEST0195 INTO W_ZLEST0195 WHERE DT_PGTO = WA_ZLEST0195-DT_PGTO.
      ENDSELECT.

      IF W_ZLEST0195 IS NOT INITIAL.
        MESSAGE 'Data de pagamento já foi cadastrada!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.

        WA_ZLEST0195-DT_ATUAL = SY-DATUM.
        WA_ZLEST0195-HR_ATUAL = SY-UZEIT.
        WA_ZLEST0195-USNAM = SY-UNAME.

        MODIFY ZLEST0195 FROM WA_ZLEST0195.

        MESSAGE 'Nova Data de pagamento incluida!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
*    WHEN 'DELETE'.
*
*      CLEAR W_ZLEST0195.
*      SELECT * FROM ZLEST0195 INTO W_ZLEST0195 WHERE DT_PGTO = WA_ZLEST0195-DT_PGTO.
*      ENDSELECT.
*      IF W_ZLEST0195 IS NOT INITIAL.
*
*        DELETE FROM ZLEST0195 WHERE DT_PGTO = WA_ZLEST0195-DT_PGTO.
*        MESSAGE 'Data de pagamento deletada!' TYPE 'S'.
*        LEAVE TO SCREEN 0.
*      ELSE.
*        MESSAGE 'Data de pagamento não foi encontrada!' TYPE 'W'.
*      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0102 OUTPUT.
  SET PF-STATUS 'PF0102'.
  SET TITLEBAR 'T0102'.
ENDMODULE.
