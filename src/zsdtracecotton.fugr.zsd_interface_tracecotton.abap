FUNCTION zsd_interface_tracecotton.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(TRACECOTTON) TYPE  ZSDT0165_T
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: it_0166 TYPE TABLE OF zsdt0166 WITH HEADER LINE,
        cont    TYPE sy-tabix.

  FREE it_0165.
  APPEND LINES OF tracecotton TO it_0165.

  LOOP AT it_0165 ASSIGNING FIELD-SYMBOL(<_tracecotton>).

    ADD 1 TO cont.

    <_tracecotton>-motivo = |{ <_tracecotton>-motivo CASE = UPPER }|.
    <_tracecotton>-algodoeira = |{ <_tracecotton>-algodoeira CASE = UPPER  }|.
    <_tracecotton>-status = |{ <_tracecotton>-status CASE = UPPER  }|.
    <_tracecotton>-tamanho_fardos = |{ <_tracecotton>-tamanho_fardos CASE = UPPER  }|.

    <_tracecotton>-id   = COND #( WHEN <_tracecotton>-id   IS INITIAL THEN zcl_tracecotton=>getseq( ) ELSE <_tracecotton>-id ).
    <_tracecotton>-data = COND #( WHEN <_tracecotton>-data IS INITIAL THEN sy-datum                   ELSE <_tracecotton>-data ).
    <_tracecotton>-hora = COND #( WHEN <_tracecotton>-hora IS INITIAL THEN sy-uzeit                   ELSE <_tracecotton>-hora ).
    <_tracecotton>-motivo = COND #( WHEN <_tracecotton>-motivo EQ 'NULL' THEN '' ELSE <_tracecotton>-motivo ).


  ENDLOOP.

  MODIFY zsdt0165 FROM TABLE it_0165.

  LOOP AT it_0165 ASSIGNING <_tracecotton>.

    "Projeto Reestruturação Algodao 2024
    "lv_werks = zcl_tracecotton=>getfazenda( <_tracecotton>-algodoeira ). "BUG 64768
    lv_werks = <_tracecotton>-codigo_filial.
    "Projeto Reestruturação Algodao 2024

    lv_bukrs = zcl_tracecotton=>getempresa( lv_werks ).

    IF zcl_tracecotton=>set_erros( <_tracecotton> ) IS INITIAL.

      APPEND VALUE #(
                      id               = <_tracecotton>-id
                      data             = sy-datum
                      hora             = sy-uzeit
                      status           = zcl_tracecotton=>getstatus( <_tracecotton>-status )
                      lote             = CONV #( <_tracecotton>-lote )
                      kunnr            = zcl_tracecotton=>get0143( contrato = CONV #( <_tracecotton>-contrato ) bukrs = lv_bukrs safra = <_tracecotton>-safra )-cliente
                      matnr            = zcl_tracecotton=>getmaterial( CONV #( <_tracecotton>-tipo ) )
                      tipo             = CONV #( <_tracecotton>-tipo )
                      qtd_fardos       = CONV #( <_tracecotton>-quantidade )
                      peso_lote        = zcl_tracecotton=>setconvertqtd( CONV #( <_tracecotton>-peso ) )
                      motivo           = <_tracecotton>-motivo
                     " WERKS            = ZCL_TRACECOTTON=>GETFAZENDA( <_TRACECOTTON>-ALGODOEIRA )
                      werks            = lv_werks
                      algodoeira       = <_tracecotton>-algodoeira "Projeto Reestruturação Algodao 2024
                      "algodoeira       = zcl_tracecotton=>getalgodoeira( werks = lv_werks lote =  <_tracecotton>-lote   safra = <_tracecotton>-safra ) "Projeto Reestruturação Algodao 2024
                      contrato         = CONV #( <_tracecotton>-contrato )
                      tamanho_fardo    = <_tracecotton>-tamanho_fardos
                      preco_ctr        = zcl_tracecotton=>get0143( contrato = CONV #( <_tracecotton>-contrato ) bukrs = lv_bukrs safra = <_tracecotton>-safra )-preco
                      data_takeup      = zcl_tracecotton=>setconvertdat( CONV #( <_tracecotton>-data_realizacao ) )
                      safra            = <_tracecotton>-safra
                      empresa          = lv_bukrs
                      " Linha abaixo comentada -  RIM-SKM-IR147418 - 18.08.2023
*                     acts             = zcl_tracecotton=>get0143( contrato = CONV #( <_tracecotton>-contrato ) bukrs = lv_bukrs safra = <_tracecotton>-safra )-acts "*-CS2023000189-05.04.2023-#108694-JT
                      usnam            = sy-uname
                      data_atual       = sy-datum
                      hora_atual       = sy-uzeit
                      ) TO it_0166.

      <_tracecotton>-rg_atualizado = '1'.

      UPDATE zsdt0178 SET status = abap_true
      WHERE id_trace EQ <_tracecotton>-id.

    ENDIF.
  ENDLOOP.

  IF cont IS INITIAL.
    result = |Não foi importado nenhum registro!|.
  ELSEIF cont > 1.
    result = |Foi importado { cont } Registro!|.
  ELSE.
    result = |Foram importados { cont } Registros!|.
  ENDIF.

  MODIFY zsdt0165 FROM TABLE it_0165.
  MODIFY zsdt0166 FROM TABLE it_0166.

ENDFUNCTION.
