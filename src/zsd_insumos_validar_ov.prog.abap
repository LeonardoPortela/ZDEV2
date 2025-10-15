*&---------------------------------------------------------------------*
*&  Include  ZSD_INSUMOS_VALIDAR_OV
*&---------------------------------------------------------------------*

************************************************************************
* valida se ha OVamarrada a algum gerador documento ativo
************************************************************************
FORM F_VALIDAR_GERADOR_DOC USING P_ERROR
                                 P_EXIT.

  DATA: W_0041     TYPE ZSDT0041,
        T_0310     TYPE TABLE OF ZSDT0310,
        W_0310_CTR TYPE ZSDT0310,
        W_0310_OVD TYPE ZSDT0310,
        T_USER     TYPE TABLE OF RGSB4,
        W_USER     TYPE RGSB4,
        L_RESP     TYPE C.

  FREE: P_EXIT.

  CHECK SY-TABIX = 1.
  CHECK P_ERROR  = ABAP_OFF.

  IF SY-TCODE IS INITIAL.
    P_EXIT = ABAP_TRUE.
    EXIT.
  ENDIF.

*-------------------------------
* ser usuarios podem cancealar doctos assinados
*-------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS           = '0000'
      SETNR           = 'ZSDT0203_USER_CANC'
      NO_DESCRIPTIONS = ABAP_OFF
    TABLES
      SET_VALUES      = T_USER
    EXCEPTIONS
      SET_NOT_FOUND   = 1
      OTHERS          = 2.

  SELECT SINGLE *
    FROM ZSDT0041
    INTO W_0041
   WHERE VBELN = VBAK-VBELN.

  CHECK SY-SUBRC = 0.

  SELECT *
    FROM ZSDT0310
    INTO TABLE T_0310
   WHERE NR_VENDA = W_0041-DOC_SIMULACAO
     AND STATUS  <> '10'.

  CHECK SY-SUBRC = 0.

  READ TABLE T_0310 INTO W_0310_OVD WITH KEY VBELN    = VBAK-VBELN
                                             TIPO_DOC = 'OVD'.

  READ TABLE T_0310 INTO W_0310_CTR WITH KEY TIPO_DOC = 'CTR'.

  IF ( W_0310_OVD-STATUS <> '00' AND W_0310_OVD-STATUS <> '01' AND W_0310_OVD-STATUS <> '02' AND
       W_0310_OVD-STATUS <> '05' AND W_0310_OVD-STATUS <> '11' AND W_0310_OVD-STATUS <> '12' AND W_0310_OVD-STATUS <> '13' ) OR
     ( W_0310_CTR IS NOT INITIAL AND
     ( W_0310_CTR-STATUS <> '00' AND W_0310_CTR-STATUS <> '01' AND W_0310_CTR-STATUS <> '02' AND
       W_0310_CTR-STATUS <> '05' AND W_0310_CTR-STATUS <> '11' AND W_0310_CTR-STATUS <> '12' AND W_0310_CTR-STATUS <> '13' ) ).

    READ TABLE T_USER INTO W_USER WITH KEY FROM = SY-UNAME.

    IF SY-SUBRC = 0 AND SY-BATCH = ABAP_FALSE.

      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
        EXPORTING
          DEFAULTOPTION  = 'N'
          DIAGNOSETEXT1  = 'Contrato / Ordem Venda com Assinatura em Andamento!'
          TEXTLINE1      = 'Deseja Realmente Cancelar este(s) Documento(s)?'
          TITEL          = 'Atenção'
          START_COLUMN   = 50
          START_ROW      = 10
          CANCEL_DISPLAY = ABAP_FALSE
        IMPORTING
          ANSWER         = L_RESP
        EXCEPTIONS
          OTHERS         = 1.

      IF L_RESP = 'N'.
        P_EXIT = ABAP_TRUE.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE I024(SD) WITH 'OV está com Documento/Contrato já '
                            'em Processo de Assinatura!'.
      P_EXIT = ABAP_TRUE.
    ENDIF.
  ENDIF.

ENDFORM.

************************************************************************
* cancela linhas da tabela de gerador documento
************************************************************************
FORM F_CANCELA_GERADOR_DOC TABLES T_0041 STRUCTURE ZSDT0041.

  DATA: W_0041     TYPE ZSDT0041,
        T_0310     TYPE TABLE OF ZSDT0310,
        W_0310_CTR TYPE ZSDT0310,
        W_0310_OVD TYPE ZSDT0310.

  CHECK T_0041[] IS NOT INITIAL.

  SELECT *
    FROM ZSDT0310
    INTO TABLE T_0310
     FOR ALL ENTRIES IN T_0041
   WHERE NR_VENDA = T_0041-DOC_SIMULACAO
     AND STATUS  <> '10'.

  CHECK SY-SUBRC = 0.

*-----------------------------------------
* solicitar cancelamento OV
*-----------------------------------------
  LOOP AT T_0310 INTO W_0310_OVD WHERE VBELN    = VBAK-VBELN
                                   AND TIPO_DOC = 'OVD'.

*-- cancela contrato / OV -------------------------------
    W_0310_OVD-STATUS     = '10'.
    W_0310_OVD-USNAM_CANC = SY-UNAME.
    W_0310_OVD-DATA_CANC  = SY-DATUM.
    W_0310_OVD-HORA_CANC  = SY-UZEIT.
    MODIFY ZSDT0310    FROM W_0310_OVD.

    DATA(L_TASK) = 'SOLICITAR_CANCELAMENTO' && W_0310_OVD-NR_VENDA && W_0310_OVD-TIPO_DOC.

    CALL FUNCTION 'ZSD_INSUMOS_CANCELAR_COLETA' STARTING NEW TASK L_TASK
      EXPORTING
        I_ID_DOCUMENTO       = W_0310_OVD-ID_DOCUMENTO
        I_CANCELAR_DOCUMENTO = ABAP_TRUE.

  ENDLOOP.

*-----------------------------------------
* solicitar cancelamento Contrato
*-----------------------------------------
  LOOP AT T_0310 INTO W_0310_CTR WHERE TIPO_DOC = 'CTR'.

*-- cancela contrato / OV -------------------------------
    W_0310_CTR-STATUS     = '10'.
    W_0310_CTR-USNAM_CANC = SY-UNAME.
    W_0310_CTR-DATA_CANC  = SY-DATUM.
    W_0310_CTR-HORA_CANC  = SY-UZEIT.
    MODIFY ZSDT0310    FROM W_0310_CTR.

    L_TASK       = 'SOLICITAR_CANCELAMENTO' && W_0310_CTR-NR_VENDA && W_0310_CTR-TIPO_DOC.

    CALL FUNCTION 'ZSD_INSUMOS_CANCELAR_COLETA' STARTING NEW TASK L_TASK
      EXPORTING
        I_ID_DOCUMENTO       = W_0310_CTR-ID_DOCUMENTO
        I_CANCELAR_DOCUMENTO = ABAP_TRUE.

  ENDLOOP.

ENDFORM.

************************************************************************
************************************************************************
