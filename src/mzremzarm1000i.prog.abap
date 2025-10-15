*----------------------------------------------------------------------*
***INCLUDE MZREMZARM1000I .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       User Command de Tela 1000
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.

  CASE OK_CODE.
    WHEN C_PSREMESSAS.
      CLEAR: OK_CODE.
      PERFORM PESQUISA_PEDIDO.
    WHEN C_PSROMANEIO.
      CLEAR: OK_CODE.
      PERFORM SELECIONAR_ROMANEIO.
    WHEN C_PSROMANEIE.
      CLEAR: OK_CODE.
      PERFORM SELECIONAR_ROMANEIO_ENTRADA.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1000  INPUT

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_PEDIDO
*&---------------------------------------------------------------------*
*       Pesquisa de Pedidos de Remessa p/ Armazenagem
*----------------------------------------------------------------------*
FORM PESQUISA_PEDIDO .

  CLEAR: IT_PEDIDOS[], IT_FORNECE[], IT_ROMANEIO[].

  SELECT CB~EBELN CB~LIFNR CB~BEDAT CB~WAERS CB~BUKRS
         IT~MATNR IT~TXZ01 IT~WERKS IT~LGORT IT~MENGE
         IT~MEINS IT~NETWR IT~NETPR IT~ELIKZ IT~MWSKZ EK~CHARG
    FROM EKKO AS CB
   INNER JOIN EKPO AS IT ON IT~EBELN EQ CB~EBELN
   LEFT OUTER JOIN EKET AS EK ON  EK~EBELN EQ IT~EBELN
                              AND EK~EBELP EQ IT~EBELP
    INTO CORRESPONDING FIELDS OF TABLE IT_PEDIDOS
   WHERE CB~EBELN IN P_EBELN
     AND CB~BUKRS IN P_BUKRS
     AND CB~LIFNR IN P_LIFNR
     AND CB~BSART IN ( 'ZARM','ZARS' ) " EQ C_ZARM
     AND IT~WERKS IN P_WERKS
     AND IT~MATNR IN P_MATNR
     AND IT~LGORT IN P_LGORT
    ORDER BY CB~EBELN DESCENDING.


  CHECK NOT IT_PEDIDOS[] IS INITIAL.


  SELECT * INTO TABLE IT_FORNECE
    FROM LFA1
     FOR ALL ENTRIES IN IT_PEDIDOS
   WHERE LIFNR EQ IT_PEDIDOS-LIFNR.

  SORT: IT_PEDIDOS BY EBELN LIFNR,
        IT_FORNECE BY LIFNR.

  IF P_CHARG IS NOT INITIAL.
    DELETE IT_PEDIDOS WHERE CHARG NOT  IN P_CHARG.
  ENDIF.
  LOOP AT IT_PEDIDOS INTO WA_PEDIDOS.
    VG_TABIX = SY-TABIX.
    READ TABLE IT_FORNECE INTO WA_FORNECE WITH KEY LIFNR = WA_PEDIDOS-LIFNR.
    IF SY-SUBRC IS INITIAL.
      WA_PEDIDOS-NAME1 = WA_FORNECE-NAME1.
      MODIFY IT_PEDIDOS INDEX VG_TABIX FROM WA_PEDIDOS TRANSPORTING NAME1.
    ENDIF.
  ENDLOOP.

  SORT: IT_PEDIDOS BY EBELN DESCENDING.
ENDFORM.                    " PESQUISA_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_ROMANEIO
*&---------------------------------------------------------------------*
*       Seleção de romaneio de saída
*----------------------------------------------------------------------*
FORM SELECIONAR_ROMANEIO .

  DATA: VG_SELECIONADO TYPE SY-SUBRC.

  CLEAR: VG_RETORNO.

  PERFORM VERIFICA_SELECAO_PEDIDO USING VG_SELECIONADO.

  IF VG_SELECIONADO IS INITIAL.
    PERFORM TELA_SELECAO_ROMANEIOS USING WA_PEDIDOS.
  ELSE.
    MESSAGE W001.
  ENDIF.

ENDFORM.                    " SELECIONAR_ROMANEIO

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_PEDIDO
*&---------------------------------------------------------------------*
*       Verifica seleção de pedido
*----------------------------------------------------------------------*
FORM VERIFICA_SELECAO_PEDIDO  USING  VG_SELECIONADO TYPE SY-SUBRC.
  READ TABLE IT_PEDIDOS INTO WA_PEDIDOS WITH KEY MARCK = C_X.
  VG_SELECIONADO = SY-SUBRC.
ENDFORM.                    " VERIFICA_SELECAO_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  TELA_SELECAO_ROMANEIOS
*&---------------------------------------------------------------------*
*       Altera para tela de seleção de romaneio
*----------------------------------------------------------------------*
FORM TELA_SELECAO_ROMANEIOS  USING  P_PEDIDOS TYPE ZARM_PEDIDO.

  RANGES: LRA_NR_ROMANEIO FOR ZSDT0001-NR_ROMANEIO.

  DATA: VL_QTD TYPE I.
  DATA VL_DOC                TYPE MKPF-MBLNR.
  DATA VL_ANO                TYPE MKPF-MJAHR.

  DATA: T_TIPO                TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.
  RANGES :  R_TIPO FOR ZSDT0001-TIPO_ENTRADA.
  IF VG_RETORNO IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ROMANEIO
      FROM ZSDT0001
     WHERE VBELN EQ P_PEDIDOS-EBELN
      AND BUKRS        EQ P_PEDIDOS-BUKRS
      AND BRANCH       EQ P_PEDIDOS-WERKS
      AND MATNR        EQ P_PEDIDOS-MATNR
      AND TP_MOVIMENTO EQ C_S
      ORDER BY NR_ROMANEIO DESCENDING.

  ELSE.
    " Usuários que podem mudar o Status
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        CLASS         = '0000'
        SETNR         = 'MAGGI_ZMM0079_TE_ENTRADA'
      TABLES
        SET_VALUES    = T_TIPO
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    SORT T_TIPO BY FROM.
    LOOP AT T_TIPO.
      R_TIPO-OPTION  = 'EQ'.
      R_TIPO-SIGN    = 'I'.
      R_TIPO-LOW     = T_TIPO-FROM.
      APPEND R_TIPO.
    ENDLOOP.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ROMANEIO
      FROM ZSDT0001
     WHERE BUKRS        EQ P_PEDIDOS-BUKRS
       AND BRANCH       EQ P_PEDIDOS-WERKS
       AND PARID        EQ P_PEDIDOS-LIFNR
       AND MATNR        EQ P_PEDIDOS-MATNR
       AND TP_MOVIMENTO EQ C_E
       AND TIPO_ENTRADA NE ''
       AND TIPO_ENTRADA IN R_TIPO
    ORDER BY NR_SAFRA DESCENDING NR_ROMANEIO DESCENDING.

  ENDIF.

  "Caso seja um range da ZMM0127, deletar da seleção, pois deve ser processado pela transação de origem
  CLEAR: LRA_NR_ROMANEIO[].
  APPEND: VALUE #( SIGN = 'I' OPTION = 'BT' LOW = 000700000 HIGH = 000799999 ) TO LRA_NR_ROMANEIO.

  DELETE IT_ROMANEIO WHERE NR_ROMANEIO IN LRA_NR_ROMANEIO.

  IF IT_ROMANEIO[] IS NOT INITIAL.
*    VG_DYNNR_000 = C_2000.

    LOOP AT IT_ROMANEIO INTO WA_ROMANEIO.

      VG_TABIX = SY-TABIX.

      VL_QTD = 0.

      IF WA_ROMANEIO-DOC_MATERIAL IS NOT INITIAL.

        SELECT COUNT(*) INTO VL_QTD
          FROM MSEG
         WHERE SMBLN EQ WA_ROMANEIO-DOC_MATERIAL.
        IF VL_QTD > 0.
          SELECT SINGLE MBLNR MJAHR INTO ( VL_DOC, VL_ANO )
             FROM MSEG
            WHERE SMBLN EQ WA_ROMANEIO-DOC_MATERIAL.
          SELECT SINGLE CPUDT CPUTM USNAM
              FROM MKPF
              INTO ( WA_ROMANEIO-DATA_MOD, WA_ROMANEIO-HORA_MOD, WA_ROMANEIO-USU_MOD )
              WHERE MBLNR = VL_DOC
              AND   MJAHR = VL_ANO.
        ELSE.
          SELECT SINGLE CPUDT CPUTM USNAM
            FROM MKPF
            INTO ( WA_ROMANEIO-DATA_MOD, WA_ROMANEIO-HORA_MOD, WA_ROMANEIO-USU_MOD )
            WHERE MBLNR = WA_ROMANEIO-DOC_MATERIAL
            AND   MJAHR = WA_ROMANEIO-ANO_MATERIAL.
        ENDIF.
        MODIFY IT_ROMANEIO INDEX VG_TABIX FROM WA_ROMANEIO TRANSPORTING DATA_MOD HORA_MOD USU_MOD.
      ENDIF.

      IF VL_QTD > 0 .
        WA_ROMANEIO-ICONE = ICON_CANCEL.
      ELSE.
        IF WA_ROMANEIO-STATUS EQ C_X.
          WA_ROMANEIO-ICONE = ICON_CHECKED.
        ELSE.
          WA_ROMANEIO-ICONE = ICON_TRANSPORT.
        ENDIF.
      ENDIF.
      IF WA_ROMANEIO-VBELN IS INITIAL.
        SELECT *
          FROM EKKO
          INTO TABLE @DATA(_T_EKKO)
          WHERE BUKRS  =  @WA_ROMANEIO-BUKRS
          AND   BSART   = 'ZARM'
          AND   LIFNR    = @WA_ROMANEIO-PARID.

        IF  _T_EKKO[] IS NOT INITIAL.
          SELECT  *
            FROM EKPO
            INTO TABLE @DATA(_T_EKPO)
            FOR ALL ENTRIES IN @_T_EKKO
            WHERE EBELN EQ @_T_EKKO-EBELN
            AND   MATNR EQ @WA_ROMANEIO-MATNR
            AND   WERKS EQ @WA_ROMANEIO-BRANCH.

          IF _T_EKPO[] IS NOT INITIAL.
            SELECT *
              FROM EKET
              INTO TABLE @DATA(_T_EKET)
              FOR ALL ENTRIES IN @_T_EKPO
              WHERE EBELN = @_T_EKPO-EBELN
              AND   EBELP = @_T_EKPO-EBELP
              AND   CHARG = @WA_ROMANEIO-NR_SAFRA.

            IF LINES( _T_EKET ) EQ 1.
              READ TABLE _T_EKET INTO DATA(_W_EKET) INDEX 1.
              WA_ROMANEIO-VBELN = _W_EKET-EBELN.
              MODIFY IT_ROMANEIO INDEX VG_TABIX FROM WA_ROMANEIO TRANSPORTING VBELN.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.

      MODIFY IT_ROMANEIO INDEX VG_TABIX FROM WA_ROMANEIO TRANSPORTING ICONE.
    ENDLOOP.

  ELSE.
    MESSAGE W002 WITH P_PEDIDOS-EBELN.
  ENDIF.
  CALL SCREEN '3000'.
ENDFORM.                    " TELA_SELECAO_ROMANEIOS

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_ROMANEIO_ENTRADA
*&---------------------------------------------------------------------*
*       Seleção de romaneio de entrada
*----------------------------------------------------------------------*
FORM SELECIONAR_ROMANEIO_ENTRADA .

  DATA: VG_SELECIONADO TYPE SY-SUBRC.
  VG_RETORNO = C_X.

  PERFORM VERIFICA_SELECAO_PEDIDO USING VG_SELECIONADO.

  IF VG_SELECIONADO IS INITIAL.
    PERFORM TELA_SELECAO_ROMANEIOS USING WA_PEDIDOS.
  ELSE.
    MESSAGE W001.
  ENDIF.

ENDFORM.                    " SELECIONAR_ROMANEIO_ENTRADA



*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_PEDIDOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TAB_PEDIDOS_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_PEDIDOS LINES TAB_PEDIDOS-LINES.
ENDMODULE.                    "TAB_PEDIDOS_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_PEDIDOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TAB_PEDIDOS_MARK INPUT.
  DATA: G_TAB_PEDIDOS_WA2 LIKE LINE OF IT_PEDIDOS.
  IF TAB_PEDIDOS-LINE_SEL_MODE = 1
  AND IT_PEDIDOS-MARCK = 'X'.
    LOOP AT IT_PEDIDOS INTO G_TAB_PEDIDOS_WA2
      WHERE MARCK = 'X'.
      G_TAB_PEDIDOS_WA2-MARCK = ''.
      MODIFY IT_PEDIDOS
        FROM G_TAB_PEDIDOS_WA2
        TRANSPORTING MARCK.
    ENDLOOP.
  ENDIF.
  MODIFY IT_PEDIDOS
    INDEX TAB_PEDIDOS-CURRENT_LINE
    TRANSPORTING MARCK.
ENDMODULE.                    "TAB_PEDIDOS_MARK INPUT
