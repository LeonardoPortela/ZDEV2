*&---------------------------------------------------------------------*
*& Report  ZPPR012
*& Controle de Defensivos - Sincronização Vencimento
*&---------------------------------------------------------------------*
*& Programa para buscar dados gravados na tabela ZPPT0018
*& com o campo REG_ATUALIZADO em branco e enviar para o 'Defensivos'
*& a nova data de vencimento.
*&---------------------------------------------------------------------*

REPORT ZPPR012.

DATA: LC_JSON TYPE STRING.

DEFINE ADD_TAG.
  CONCATENATE LC_JSON '"' &1 '" : "' &2 '"' &3 INTO LC_JSON.
END-OF-DEFINITION.

START-OF-SELECTION.

  PERFORM ATUALIZA_DATAS.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DATAS
*&---------------------------------------------------------------------*
FORM ATUALIZA_DATAS.
  DATA: TABIX TYPE SY-TABIX.

  CLEAR: LC_JSON.  DATA:  OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.

  SELECT * FROM ZPPT0018 INTO TABLE @DATA(T_0018) WHERE REG_ATUALIZADO = @SPACE.

  DATA(LEN) = LINES( T_0018[] ).

  IF ( SY-SUBRC EQ 0 ).

    CONCATENATE '{' LC_JSON INTO LC_JSON.
    CONCATENATE LC_JSON 'Lotes:[' INTO LC_JSON.


    LOOP AT T_0018[] ASSIGNING FIELD-SYMBOL(<W_0018>).
      TABIX = SY-TABIX.

      <W_0018>-MATNR  = |{ <W_0018>-MATNR ALPHA = OUT }|.
      DATA(_VFDAT)    = |{ <W_0018>-VFDAT+0(4) }-{ <W_0018>-VFDAT+4(2) }-{ <W_0018>-VFDAT+6(2) }|.

      CONCATENATE LC_JSON '{' INTO LC_JSON.
      ADD_TAG 'filial'          <W_0018>-WERKS  ','.
      ADD_TAG 'deposito'        <W_0018>-LGORT  ','.
      ADD_TAG 'codigoMaterial'  <W_0018>-MATNR  ','.
      ADD_TAG 'loteIndividual'  <W_0018>-CHARG  ','.
      ADD_TAG 'data'            _VFDAT ' '.
      CONCATENATE LC_JSON '}' INTO LC_JSON.

      IF LINES( T_0018[] ) NE 1 AND TABIX NE LEN.
        CONCATENATE LC_JSON ',' INTO LC_JSON.
      ENDIF.

      <W_0018>-REG_ATUALIZADO = ABAP_TRUE.
      <W_0018>-USNAM          = SY-UNAME.
      <W_0018>-DATA_ATUAL     = SY-DATUM.
      <W_0018>-HORA_ATUAL     = SY-UZEIT.

    ENDLOOP.

    CONCATENATE LC_JSON ']' INTO LC_JSON.
    CONCATENATE LC_JSON '}' INTO LC_JSON.

    CREATE OBJECT OB_WEB_SERVICE.

    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'DV' ).
        OB_WEB_SERVICE->SET_TIPO( I_TIPO = 'D' ).
      CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
    ENDTRY.

    TRY .
        DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( ).
        DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
    ENDTRY.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP ).

    CALL METHOD OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR
      EXPORTING
        I_HTTP                     = VAR_HTTP
        I_XML                      = LC_JSON
      RECEIVING
        E_RESULTADO                = DATA(JSON_RETORNO)
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5.

    IF ( SY-SUBRC = 0 ).
      IF ( JSON_RETORNO CS 'true' ).
        MODIFY ZPPT0018 FROM TABLE T_0018.
      ENDIF.
    ENDIF.


  ENDIF.

ENDFORM.
