CLASS zcl_descarga_cd_luft DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_lf TYPE c LENGTH 2 VALUE 'LF'.

    METHODS processa
      IMPORTING
        in_input TYPE zmms_req_descargas_in
      EXPORTING
        ev_ok    TYPE boolean
        ev_msg   TYPE string .
  PROTECTED SECTION.
private section.

  data:
    gt_header TYPE TABLE OF zmmt0205 .
  data:
    gt_items TYPE TABLE OF zmmt0206 .

  methods VALIDA_DADOS
    importing
      !IN_INPUT type ZMMS_REQ_DESCARGAS_IN
    exporting
      !EV_OK type BOOLEAN
      !EV_MSG type STRING .
  methods SPLIT_TABELAS
    importing
      !IN_INPUT type ZMMS_REQ_DESCARGAS_IN .
  methods SALVAR_TABELAS .
  methods VERIFICA_NF_ATIVAS
    importing
      !IV_NF type ZDE_CHAVE_DOC_E
    exporting
      !IV_MOV type BOOLEAN .
ENDCLASS.



CLASS ZCL_DESCARGA_CD_LUFT IMPLEMENTATION.


  METHOD processa.

    CLEAR: ev_msg, ev_ok.

    valida_dados(
      EXPORTING
        in_input = in_input
      IMPORTING
        ev_ok    = ev_ok
        ev_msg   = ev_msg
    ).

    IF ev_msg IS INITIAL.

      split_tabelas( in_input ).

      salvar_tabelas( ).

      ev_ok = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD valida_dados.

    DATA: lva_lifnr TYPE lfa1-lifnr.

    LOOP AT in_input-descargas ASSIGNING FIELD-SYMBOL(<fs_descarga>).

      IF <fs_descarga>-cabecalho_chegada-data_chegada  IS INITIAL OR
         <fs_descarga>-cabecalho_chegada-data_descarga IS INITIAL OR
         "<fs_descarga>-cabecalho_chegada-placa IS INITIAL OR
         <fs_descarga>-cabecalho_chegada-tipo_insumo   IS INITIAL.

        ev_msg = 'Dados cabeçalho chegada faltante'.
        ev_ok = abap_false.
        RETURN.

      ENDIF.

      IF <fs_descarga>-cabecalho_chegada-chave_nfe IS INITIAL.
        ev_msg = 'Chave NF-e no cabeçalho chegada não informado!'.
        ev_ok = abap_false.
        RETURN.
      ENDIF.


      IF strlen( <fs_descarga>-cabecalho_chegada-chave_nfe ) NE 44.
        ev_msg = 'Tamanho Inválido da Chave NF-e no cabeçalho Chegada!'.
        ev_ok = abap_false.
        RETURN.
      ENDIF.


      IF <fs_descarga>-cabecalho_chegada-id_cd IS NOT INITIAL.

        lva_lifnr = <fs_descarga>-cabecalho_chegada-id_cd.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lva_lifnr
          IMPORTING
            output = lva_lifnr.

        SELECT SINGLE *
          FROM lfa1 INTO @DATA(lwa_lfa1)
         WHERE lifnr EQ @lva_lifnr.

        IF sy-subrc IS NOT INITIAL.
          ev_msg = | Id CD { lva_lifnr } não localizado como codigo fornecedor!|.
          ev_ok = abap_false.
          RETURN.
        ENDIF.

      ENDIF.

      IF <fs_descarga>-cabecalho_chegada-tipo_insumo NE 'SM' AND <fs_descarga>-cabecalho_chegada-tipo_insumo NE 'DF'.
        ev_msg = |Tipo Insumo { <fs_descarga>-cabecalho_chegada-tipo_insumo } inválido! Só permitido SM ou DF!|.
        ev_ok = abap_false.
        RETURN.
      ENDIF.

      IF <fs_descarga>-itens_chegada IS INITIAL.

        ev_msg = 'Dados items da chegada vazios'.
        ev_ok = abap_false.
        RETURN.
      ELSE.

        LOOP AT <fs_descarga>-itens_chegada ASSIGNING FIELD-SYMBOL(<fs_item>).

          IF <fs_item>-id_item_chegada IS INITIAL.
            ev_msg = 'Id. Item Chegada vazio!'.
            ev_ok = abap_false.
            RETURN.
          ENDIF.

          IF <fs_item>-codigo_produto_amaggi IS INITIAL.
            ev_msg = 'Codigo Produto Amaggi do item não pode ser Vazio!'.
            ev_ok = abap_false.
            RETURN.
          ENDIF.

          IF <fs_item>-quantidade IS INITIAL.
            ev_msg = 'Quantidade do item não pode ser Vazio!'.
            ev_ok = abap_false.
            RETURN.
          ENDIF.

          IF <fs_item>-lote IS INITIAL.
            ev_msg = 'Lote do item não pode ser Vazio!'.
            ev_ok = abap_false.
            RETURN.
          ENDIF.


*          IF <fs_descarga>-cabecalho_chegada-tipo_insumo EQ 'SM'.
*
*            SELECT COUNT(*)
*              FROM zmmt0208
*             WHERE tipo_semente = @<fs_item>-tipo_semente.
*
*            IF sy-subrc <> 0.
*              ev_msg = 'Tipo de semente não cadastrada'.
*              ev_ok = abap_false.
*              RETURN.
*            ENDIF.
*
*            SELECT COUNT(*)
*              FROM zmmt0209
*             WHERE embalagem = @<fs_item>-embalagem.
*
*            IF sy-subrc <> 0.
*              ev_msg = 'Tipo de embalagem não cadastrada'.
*              ev_ok = abap_false.
*              RETURN.
*            ENDIF.
*
*          ENDIF.

        ENDLOOP.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD split_tabelas.


    LOOP AT in_input-descargas ASSIGNING FIELD-SYMBOL(<fs_descarga>).

      verifica_nf_ativas( EXPORTING iv_nf = <fs_descarga>-cabecalho_chegada-chave_nfe
                          IMPORTING iv_mov = DATA(lv_mov) ).

      APPEND INITIAL LINE TO gt_header ASSIGNING FIELD-SYMBOL(<fs_hd>).

      "Cabeçalho
      <fs_hd> = CORRESPONDING #( <fs_descarga>-cabecalho_chegada  ).

      IF <fs_hd>-id_cd IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_hd>-id_cd
          IMPORTING
            output = <fs_hd>-id_cd.
      ELSE.
        <fs_hd>-cd_descarga = c_lf. "LUFT
      ENDIF.

      GET TIME STAMP FIELD <fs_hd>-id_chegada_cd_luft.
      <fs_hd>-user_create = sy-uname.
      <fs_hd>-date_create = sy-datum.
      <fs_hd>-time_create = sy-uzeit.

      IF lv_mov IS NOT INITIAL.
        <fs_hd>-receb_migo_gerada = abap_true.

        CLEAR lv_mov.
      ENDIF.

      "Itens
      LOOP AT <fs_descarga>-itens_chegada ASSIGNING FIELD-SYMBOL(<fs_it>).

        APPEND INITIAL LINE TO gt_items ASSIGNING FIELD-SYMBOL(<fs_item>).

        <fs_item> = CORRESPONDING #( <fs_it>  ).
        <fs_item>-id_chegada_cd_luft = <fs_hd>-id_chegada_cd_luft.
        <fs_item>-user_create = sy-uname.
        <fs_item>-date_create = sy-datum.
        <fs_item>-time_create = sy-uzeit.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = <fs_item>-codigo_produto_amaggi
          IMPORTING
            output = <fs_item>-codigo_produto_amaggi.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD salvar_tabelas.

    MODIFY zmmt0205 FROM TABLE gt_header.
    MODIFY zmmt0206 FROM TABLE gt_items.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDMETHOD.


  METHOD verifica_nf_ativas.

    CHECK iv_nf IS NOT INITIAL.

    "Busca nota ativa
    SELECT SINGLE hd~id_chegada_cd_luft, hd~chave_nfe, hd~receb_migo_gerada
       FROM zmmt0205 AS hd
       WHERE hd~chave_nfe = @iv_nf
         AND hd~cancel = ''
       INTO @DATA(ls_nf_tab).

    CHECK sy-subrc EQ 0.

    "#173918 / RG - 30.04.2025
    "Verifica se existe movimentação para a nota ativa
    SELECT chave_nfe, prod_item, mblnr, docnum_nfe, belnr_ft
      FROM zib_nfe_dist_itm
      INTO TABLE @DATA(lt_nfe_mov)
      WHERE chave_nfe = @iv_nf.

    DELETE lt_nfe_mov WHERE mblnr IS INITIAL AND docnum_nfe IS INITIAL AND belnr_ft IS INITIAL.

    DATA(_mov_gerado_nfe) = abap_false.
    IF lt_nfe_mov[] IS NOT INITIAL .
      _mov_gerado_nfe = abap_true.
    ENDIF.

    CASE _mov_gerado_nfe.
      WHEN abap_true.  "Tem Movimento Gerado

        iv_mov = abap_true.

        UPDATE zmmt0205 SET user_cancel = sy-uname
                            date_cancel = sy-datum
                            time_cancel = sy-uzeit
                            cancel = abap_true
         WHERE chave_nfe         = iv_nf
           AND receb_migo_gerada = abap_true.

      WHEN abap_false. "Não Tem Movimento Gerado

        UPDATE zmmt0205 SET user_cancel = sy-uname
                            date_cancel = sy-datum
                            time_cancel = sy-uzeit
                            cancel = abap_true
          WHERE chave_nfe = iv_nf.

        UPDATE zib_nfe_dist_itm SET ebeln = '',
                                    ebelp = '',
                                    matnr = '',
                                    menge = '',
                                    meins = ''
         WHERE chave_nfe = @iv_nf.

        UPDATE zib_nfe_dist_ter SET st_fiscal = '',
                                    st_fisico = ''
                              WHERE chave_nfe = @iv_nf.

*** Inicio - Rubenilson - 12.06.2025 - #173494
        UPDATE zmmt0218 SET cancel = abap_true
                            user_cancel = sy-uname
                            date_cancel = sy-datum
                            time_cancel = sy-uzeit
                      WHERE chave_nfe = iv_nf.
*** Fim - Rubenilson - 12.06.2025 - #173494
    ENDCASE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.




  ENDMETHOD.
ENDCLASS.
