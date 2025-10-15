FUNCTION z_mm_criar_aviso.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(WA_MOV_ESTQ) TYPE  ZMMT_EE_ZGR OPTIONAL
*"     REFERENCE(INVOICE_DOC) TYPE  RE_BELNR OPTIONAL
*"     REFERENCE(INVOICE_ANO) TYPE  GJAHR OPTIONAL
*"     REFERENCE(DATA_AVISO) TYPE  DATUM OPTIONAL
*"     REFERENCE(DATA_LACTO) TYPE  DATUM OPTIONAL
*"     VALUE(GERAR_MIGO) TYPE  CHAR01 OPTIONAL
*"     VALUE(GERAR_APENAS_AVISO) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(WA_ZLEST0108) TYPE  ZLEST0108 OPTIONAL
*"     REFERENCE(WA_ZLEST0110) TYPE  ZLEST0110 OPTIONAL
*"     REFERENCE(FRETE_ENT_TERC) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(DOC_AVISO_GER) TYPE  ZLEST0109 OPTIONAL
*"  EXPORTING
*"     VALUE(DOC_GERADOS) TYPE  ZMMT_EE_ZGR_DOCS
*"  TABLES
*"      IT_OUT STRUCTURE  ZFIE_RET_DOCUMENT OPTIONAL
*"      IT_ZLEST0109 STRUCTURE  ZLEST0109 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CONSTANTS: c_e(1)     TYPE c VALUE 'E',
             c_x(1)     TYPE c VALUE 'X',
             c_n(1)     TYPE c VALUE 'N',
             c_10(2)    TYPE c VALUE '10',
             c_vl31n(5) TYPE c VALUE 'VL31N',
             c_la(2)    TYPE c VALUE 'LA',
             c_vl32n(5) TYPE c VALUE 'VL32N',
             c_00010(5) TYPE c VALUE '00010',
             c_0001(4)  TYPE c VALUE '0001',
             c_160(3)   TYPE c VALUE '160',
             c_311(3)   TYPE c VALUE '311'.

  DATA: vl_data          TYPE dats,
        vl_data_em       TYPE dats,
        vl_lfimg(20)     TYPE c,
        vl_brgew(20)     TYPE c,
        it_bdcdata       TYPE TABLE OF bdcdata,
        vl_mode          TYPE c,
        vg_erro          TYPE c LENGTH 1,
        it_msg           TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        it_ekes          TYPE TABLE OF ekes WITH HEADER LINE,
        wa_mov_estq_docs TYPE zmmt_ee_zgr_docs,
        it_xekes_aux     TYPE TABLE OF uekes,
        it_yekes         TYPE TABLE OF uekes,
        wa_xekes         TYPE uekes,
        vg_doc_date      LIKE zmmt_ee_zgr-doc_date,
        vg_pstng_date    LIKE zmmt_ee_zgr-pstng_date,
        vl_nf_serie      TYPE string,
        vl_desc_nf       TYPE string,
        vl_desc_item     TYPE string,
        vl_cont_item(2)  TYPE c,
        vl_idx_item      TYPE i,
        vl_vlr_nf(20)    TYPE c,
        wa_zlest0108_rec TYPE zlest0108,
        wa_zlest0109     TYPE zlest0109,
        wa_zlest0110_rec TYPE zlest0110,
        wa_lfa1          TYPE lfa1,
        wa_kna1          TYPE kna1,
        wa_trolz         TYPE trolz,
        wa_tvro          TYPE tvro,
        vl_msg_tmp       TYPE string,
        vl_ds_aviso_rec  TYPE string,
        wl_mkpf_ger      TYPE mkpf,
        vinco1           TYPE likp-inco1.

  IF ( invoice_doc IS NOT INITIAL ) AND ( invoice_ano IS NOT INITIAL ).
    CLEAR: it_out.
    IF wa_mov_estq-tp_operacao EQ '08'.
      SELECT SINGLE * INTO wa_mov_estq_docs
        FROM zmmt_ee_zgr_docs
       WHERE mm_mblnr EQ invoice_doc
         AND mm_mjahr EQ invoice_ano.
    ELSE.
      SELECT SINGLE * INTO wa_mov_estq_docs
        FROM zmmt_ee_zgr_docs
       WHERE ft_belnr EQ invoice_doc
         AND ft_gjahr EQ invoice_ano.
    ENDIF.
  ELSEIF ( NOT wa_mov_estq IS INITIAL ) AND ( invoice_doc IS INITIAL ).
    gerar_migo = 'X'.
  ENDIF.

*  IF WA_ZLEST0110-CHAVE IS NOT INITIAL AND WA_ZLEST0110-NFE EQ ABAP_TRUE AND ( WA_ZLEST0110-ENTRAD EQ ABAP_FALSE ).
*
*    ZCL_NFE_XML=>ZIF_NFE_XML~GET_INSTANCE(
*      )->SET_REGISTRO( I_CHAVE = WA_ZLEST0110-CHAVE
*      )->GET_VALIDAR( EXPORTING I_MATERIAL =  WA_ZLEST0110-MATERIAL IMPORTING E_VALIDACAO = DATA(E_VALIDACAO)
*      ).
*
*    IF E_VALIDACAO-CK_ERRO EQ ABAP_TRUE.
*      DATA: LC_TEXTO TYPE C LENGTH 200.
*      LC_TEXTO = E_VALIDACAO-DS_MESSAGEM.
*      SY-MSGV1 = LC_TEXTO+000(50).
*      SY-MSGV2 = LC_TEXTO+050(50).
*      SY-MSGV3 = LC_TEXTO+100(50).
*      SY-MSGV4 = LC_TEXTO+150(50).
*
*      MESSAGE ID ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID TYPE 'S'
*       NUMBER ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
*      RAISING ERROR.
*
*    ELSEIF E_VALIDACAO-NM_QTD_ITENS GT 1.
*      MESSAGE ID ZCX_CARGA=>ZCX_NFE_MANY_ROWS-MSGID TYPE 'S' NUMBER ZCX_CARGA=>ZCX_NFE_MANY_ROWS-MSGNO RAISING ERROR.
*    ENDIF.
*
*  ENDIF.

  IF data_aviso IS NOT INITIAL.
    vg_doc_date = data_aviso.
  ELSE.
    vg_doc_date = wa_mov_estq-doc_date.
  ENDIF.

  IF data_lacto IS NOT INITIAL.
    vg_pstng_date = data_lacto.
  ELSE.
    vg_pstng_date = wa_mov_estq-pstng_date.
  ENDIF.

  "Cockpit de Frete - Entrada
  IF frete_ent_terc IS NOT INITIAL.

    CLEAR: vl_nf_serie, vl_desc_nf, wa_zlest0109, vl_vlr_nf, wa_lfa1, wa_kna1, wa_trolz, wa_tvro.

    wa_zlest0108_rec = wa_zlest0108.
    wa_zlest0110_rec = wa_zlest0110.

    READ TABLE it_zlest0109 INTO wa_zlest0109 INDEX 1.

    CONCATENATE 'Modelo:' wa_zlest0110_rec-modelo '-'
                'Serie:'  wa_zlest0110_rec-serie  '-'
                'Numero:' wa_zlest0110_rec-numero INTO vl_desc_nf.

    CONCATENATE wa_zlest0110_rec-numero '-' wa_zlest0110_rec-serie INTO vl_nf_serie.

    vl_vlr_nf = wa_zlest0110_rec-vl_nota_fiscal.

    SHIFT vl_vlr_nf LEFT DELETING LEADING space.
    TRANSLATE vl_vlr_nf USING '.,'.

    SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = wa_zlest0108_rec-cod_loc_coleta.
    SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr = wa_zlest0108_rec-cod_loc_entrega.

    SELECT SINGLE *
      INTO wa_trolz
      FROM trolz
     WHERE aland = 'BR'
       AND azone = wa_lfa1-lzone
       AND lland = 'BR'
       AND lzone = wa_kna1-lzone.

    SELECT SINGLE * INTO wa_tvro FROM tvro WHERE route = wa_trolz-route.

  ENDIF.
  "Fim Cockpit de Frete - Entrada

  CONCATENATE vg_doc_date+6(2)   vg_doc_date+4(2)   vg_doc_date(4)   INTO vl_data.
  CONCATENATE vg_pstng_date+6(2) vg_pstng_date+4(2) vg_pstng_date(4) INTO vl_data_em.

  vl_lfimg = wa_mov_estq-entry_qnt.
  vl_brgew = wa_mov_estq-peso_bruto.

  SHIFT vl_lfimg LEFT DELETING LEADING space.
  TRANSLATE vl_lfimg USING '.,'.

  SHIFT vl_brgew LEFT DELETING LEADING space.
  TRANSLATE vl_brgew USING '.,'.
  CONDENSE vl_brgew.

  CLEAR it_bdcdata.

  IF ( frete_ent_terc IS NOT INITIAL ). "Se a chamada for pelo Cockpit de Frete - Entrada (ZLES0113)

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '4007',
                                   ' ' 'BDC_CURSOR'         'RV50A-VERUR_LA',
                                   ' ' 'BDC_OKCODE'         '/00',
                                   ' ' 'LIKP-LIFNR'	        wa_zlest0109-lifnr,
                                   ' ' 'LV50C-BSTNR'        wa_zlest0109-ebeln,
                                   ' ' 'RV50A-LFDAT_LA'     vl_data,
                                   ' ' 'RV50A-LFUHR_LA'     '00:00',
                                   ' ' 'RV50A-VERUR_LA'     vl_nf_serie.

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '1000',
                                   ' ' 'BDC_OKCODE'         '=T\01',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'LIKP-BLDAT'         vl_data,
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_CURSOR'         'LIPS-MATNR(02)',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A'.
    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '1000',
                                   ' ' 'BDC_OKCODE'         '=HDET_T',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'LIKP-BLDAT'         vl_data,
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_CURSOR'         'LIPSD-G_LFIMG(01)',
                                   ' ' 'RV50A-LFDAT_LA'     vl_data,
                                   ' ' 'RV50A-LFUHR_LA'     '00:00'.

    "Atribui Itens
    vl_idx_item = 1.
    LOOP AT it_zlest0109 INTO wa_zlest0109.

      CLEAR: vl_desc_item, vl_cont_item.

      vl_cont_item = vl_idx_item.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_cont_item
        IMPORTING
          output = vl_cont_item.

      ADD 1 TO vl_idx_item.

      vl_lfimg = wa_zlest0109-qtde_aviso.

      SHIFT vl_lfimg LEFT DELETING LEADING space.
      TRANSLATE vl_lfimg USING '.,'.

      CONCATENATE 'LIPSD-G_LFIMG('  vl_cont_item  ')' INTO vl_desc_item.

      PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: ' ' vl_desc_item  vl_lfimg.

      IF wa_zlest0109-lgort IS NOT INITIAL.
        CONCATENATE 'LIPS-LGORT('  vl_cont_item  ')' INTO vl_desc_item.
        PERFORM z_preenche_dbc TABLES it_bdcdata
                                USING: ' ' vl_desc_item  wa_zlest0109-lgort.
      ENDIF.

    ENDLOOP.

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A'.

    SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr = wa_zlest0108_rec-agente_frete.
    IF wa_lfa1-ktokk = 'ZFIC'.
      vinco1 = 'CIF'.
    ELSE.
      vinco1 = 'CPT'.
    ENDIF.

    PERFORM z_preenche_dbc TABLES it_bdcdata
                             USING: 'X' 'SAPMV50A'           '2000',
                                    ' ' 'BDC_OKCODE'         '=T\03',
                                    ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                    ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                    ' ' 'BDC_CURSOR'          'LIKP-TDDAT',
                                    ' ' 'RV50A-LFDAT_LA'     vl_data,
                                    ' ' 'RV50A-LFUHR_LA'     '00:00',
                                    ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                    ' ' 'BDC_SUBSCR'         'SAPMV50A'.

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '2000',
                                   ' ' 'BDC_OKCODE'         '/00',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_CURSOR'         'LIKP-ROUTE',
                                   ' ' 'LIKP-ROUTE'	        wa_trolz-route,
                                   ' ' 'LIKP-INCO1'	        vinco1,
                                   ' ' 'LIKP-BOLNR'	        vl_vlr_nf,
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A'.

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '2000',
                                   ' ' 'BDC_OKCODE'         '=T\07',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_CURSOR'	        'LIKP-TDDAT',
                                   ' ' 'RV50A-LFDAT_LA'     vl_data,
                                   ' ' 'RV50A-LFUHR_LA'     '00:00',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A'.

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '2000',
                                   ' ' 'BDC_OKCODE'         '/00',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'	        'SAPLV09C',
                                   ' ' 'GV_FILTER'          'PARALL',
                                   ' ' 'GVS_TC_DATA-REC-PARVW(02)'  'SP',
                                   ' ' 'GVS_TC_DATA-REC-PARVW(03)'  'LR',
                                   ' ' 'GVS_TC_DATA-REC-PARVW(04)'  'PC',
                                   ' ' 'GVS_TC_DATA-REC-PARTNER_EXT(02)'  wa_zlest0108_rec-agente_frete,
                                   ' ' 'GVS_TC_DATA-REC-PARTNER_EXT(03)'  wa_zlest0108_rec-cod_dest_merc,
                                   ' ' 'GVS_TC_DATA-REC-PARTNER_EXT(04)'  wa_zlest0108_rec-cod_loc_coleta.

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '2000',
                                   ' ' 'BDC_OKCODE'         '=SICH_T',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'GV_FILTER'          'PARALL'.


  ENDIF.

  IF ( frete_ent_terc IS INITIAL ) AND "Se a chamada NÃO for pela ZLES0113
     ( doc_aviso_ger  IS INITIAL ).    "Se o Aviso de Rec. já não foi criado pela ZLES0113(doc_aviso_ger is initial)

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '4007',
                                   ' ' 'BDC_CURSOR'         'LV50C-BSTNR',
                                   ' ' 'BDC_OKCODE'         '/00',
                                   ' ' 'LV50C-BSTNR'         wa_mov_estq-po_number,
                                   ' ' 'RV50A-LFDAT_LA'     vl_data.
    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '1000',
                                   ' ' 'BDC_OKCODE'         '/00',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'LIKP-BLDAT'         vl_data,
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_CURSOR'         'LIPSD-G_LFIMG(01)',
                                   ' ' 'RV50A-LFDAT_LA'     vl_data,
                                   ' ' 'RV50A-WADAT_IST_LA' vl_data_em,

                                   ' ' 'LIPSD-G_LFIMG(01)'  vl_lfimg.  "*****

    IF ( wa_mov_estq-peso_bruto > 0 ) AND ( vl_brgew IS NOT INITIAL ) AND
       ( wa_mov_estq-peso_bruto > wa_mov_estq-entry_qnt  ).
      PERFORM z_preenche_dbc TABLES it_bdcdata
                              USING: 'X' 'SAPMV50A'           '1000',
                                     ' ' 'BDC_OKCODE'         '/00',
                                     ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                     ' ' 'LIKP-BLDAT'         vl_data,
                                     ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                     ' ' 'BDC_CURSOR'         'LIPS-BRGEW(01)',
                                     ' ' 'LIPS-BRGEW(01)'     vl_brgew.
    ENDIF.

    IF wa_mov_estq-lgort IS NOT INITIAL.
      PERFORM z_preenche_dbc TABLES it_bdcdata
                              USING: ' ' 'BDC_CURSOR'         'LIPS-LGORT(01)',
                                     ' ' 'LIPS-LGORT(01)'     wa_mov_estq-lgort.
    ENDIF.

    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A'.
    PERFORM z_preenche_dbc TABLES it_bdcdata
                            USING: 'X' 'SAPMV50A'           '1000',
                                   ' ' 'BDC_OKCODE'         '=SICH_T',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'LIKP-BLDAT'         vl_data,
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_CURSOR'         'LIPS-MATNR(02)',
                                   ' ' 'RV50A-LFDAT_LA'     vl_data,
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A',
                                   ' ' 'BDC_SUBSCR'         'SAPMV50A'.

  ENDIF.



*  perform z_preenche_dbc tables it_bdcdata
*                         using: 'X' 'SAPMV50A'       '4007',
*                                ' ' 'BDC_CURSOR'     'RV50A-LFDAT_LA',
*                                ' ' 'BDC_OKCODE'     '/00',
*                                ' ' 'LV50C-BSTNR'     wa_mov_estq-po_number,
*                                ' ' 'RV50A-LFDAT_LA' vl_data.
*
*  perform z_preenche_dbc tables it_bdcdata
*                         using: 'X' 'SAPMV50A'   '1000',
*                                ' ' 'BDC_OKCODE' '=T\01'.
*
*  perform z_preenche_dbc tables it_bdcdata
*                         using: 'X' 'SAPMV50A'           '1000',
*                                ' ' 'BDC_OKCODE'         '=T\03',
*                                ' ' 'BDC_SUBSCR'         'SAPMV50A',
*                                ' ' 'LIKP-BLDAT'         vl_data,
*                                ' ' 'BDC_SUBSCR'         'SAPMV50A',
*                                ' ' 'BDC_CURSOR'         'LIPS-VRKME(01)',
*                                ' ' 'RV50A-LFDAT_LA'     vl_data,
*                                ' ' 'RV50A-WADAT_IST_LA' vl_data_em,
*                                ' ' 'BDC_SUBSCR'         'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'         'SAPMV50A'.
*
*  perform z_preenche_dbc tables it_bdcdata
*                         using: 'X' 'SAPMV50A'          '1000',
*                                ' ' 'BDC_OKCODE'        '/00',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_CURSOR'        'LIPS-GEWEI(01)',
*                                ' ' 'LIPSD-G_LFIMG(01)' vl_lfimg,
*                                ' ' 'LIPS-VRKME(01)'    wa_mov_estq-meins,
*                                ' ' 'LIPS-BRGEW(01)'    vl_brgew,
*                                ' ' 'LIPS-GEWEI(01)'    wa_mov_estq-meins,
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A'.
*
*  perform z_preenche_dbc tables it_bdcdata
*                         using: 'X' 'SAPMV50A'          '1000',
*                                ' ' 'BDC_OKCODE'        '=SICH_T',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_CURSOR'        'LIPS-GEWEI(01)',
*                                ' ' 'LIPSD-G_LFIMG(01)' vl_lfimg,
*                                ' ' 'LIPS-VRKME(01)'    wa_mov_estq-meins,
*                                ' ' 'LIPS-BRGEW(01)'    vl_brgew,
*                                ' ' 'LIPS-GEWEI(01)'    wa_mov_estq-meins,
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A'.

  vl_mode = c_n.

  IF doc_aviso_ger IS INITIAL. "Se o Aviso de Rec. já não foi criado pela ZLES0113(doc_aviso_ger is initial)
    "Chama a transação VL31N (Criação)
    CALL TRANSACTION c_vl31n USING it_bdcdata MODE vl_mode UPDATE c_s MESSAGES INTO it_msg.
  ENDIF.

**Verifica se houve algum erro no processo da VL31N
  READ TABLE it_msg WITH KEY msgtyp = c_e.
  IF sy-subrc = 0.
    vg_erro = c_x.
  ELSE.
    CLEAR vg_erro.


    IF doc_aviso_ger IS INITIAL. "Se o Aviso de Rec. já não foi criado pela ZLES0113(doc_aviso_ger is initial)

      "Verifica a msg de sucesso com o número criado
      READ TABLE it_msg WITH KEY msgtyp = c_s msgnr = c_311.
      IF sy-subrc IS INITIAL.
        PERFORM add_mensagem TABLES it_out USING wa_mov_estq it_msg c_10 vl_desc_nf.
      ENDIF.

      MOVE it_msg-msgv2 TO doc_gerados-av_vbeln.

    ELSE.

      CLEAR: vl_ds_aviso_rec.
      CONCATENATE 'Recebimento' doc_aviso_ger-vbeln 'gravado(s)'
             INTO vl_ds_aviso_rec SEPARATED BY space.

      PERFORM z_prepara_mensagem TABLES it_out
             USING wa_mov_estq-obj_key
                   'S'
                   vl_ds_aviso_rec
                   doc_aviso_ger-vbeln
                   vl_desc_nf
                   c_10.

      MOVE doc_aviso_ger-vbeln TO doc_gerados-av_vbeln.

    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = doc_gerados-av_vbeln
      IMPORTING
        output = doc_gerados-av_vbeln.

    sy-subrc = 1.

    IF ( ( invoice_doc IS INITIAL ) OR ( gerar_migo IS NOT INITIAL AND NOT invoice_doc IS INITIAL ) ) AND ( gerar_apenas_aviso NE 'X' ).

      CLEAR: wl_mkpf_ger.

      "Verificar se já foi feito piking
      SELECT SINGLE * INTO @DATA(wa_ekko)
        FROM ekko
       WHERE ebeln EQ @wa_mov_estq-po_number.

      IF ( sy-subrc EQ 0 ) AND ( wa_mov_estq-po_number IS NOT INITIAL ) AND ( wa_mov_estq-nt_remessa IS NOT INITIAL ).
        "Verificar se Documento Está lançado e Não Estornado
        wl_mkpf_ger = zcl_migo=>get_migo_pedido_valida( i_ebeln = CONV #( wa_mov_estq-po_number  )
                                                        i_xblnr = CONV #( wa_mov_estq-nt_remessa ) ).

        IF wl_mkpf_ger IS NOT INITIAL.
          vg_erro = c_x.

          CLEAR: vl_msg_tmp.
          CONCATENATE 'Documento Material:' wl_mkpf_ger-mblnr wl_mkpf_ger-mjahr 'já foi gerado para a NF:' wa_mov_estq-nt_remessa INTO vl_msg_tmp SEPARATED BY space.

          PERFORM z_prepara_mensagem TABLES it_out
            USING wa_mov_estq-obj_key
                  'E'
                  vl_msg_tmp
                  ' '
                  ' '
                  c_10.
        ENDIF.
      ENDIF.

      REFRESH it_bdcdata.
      CLEAR it_msg.
      PERFORM z_preenche_dbc TABLES it_bdcdata
                             USING: 'X'  'SAPMV50A'    '4104',
                                    ' '  'BDC_OKCODE'  '/00',
                                    ' '  'LIKP-VBELN'  doc_gerados-av_vbeln.

      CONCATENATE wa_mov_estq-pstng_date+6(2)   wa_mov_estq-pstng_date+4(2)   wa_mov_estq-pstng_date(4)   INTO vl_data.

      PERFORM z_preenche_dbc TABLES it_bdcdata
                             USING: 'X'  'SAPMV50A'           '1000',
                                    ' '  'BDC_OKCODE'         '=T\01'.

      PERFORM z_preenche_dbc TABLES it_bdcdata
                             USING: 'X'  'SAPMV50A'           '1000',
                                    ' '  'RV50A-WADAT_IST_LA' vl_data,
                                    ' '  'BDC_OKCODE'         '=WABU_T'.

      vl_mode = c_n.

***Chama a transação VL32N
*** picking
      IF wl_mkpf_ger IS INITIAL.
        CALL TRANSACTION c_vl32n USING it_bdcdata MODE vl_mode UPDATE c_s MESSAGES INTO it_msg.

        "Verifica se houve algum erro no processo da VL32N
        READ TABLE it_msg WITH KEY msgtyp = c_e.

        IF sy-subrc = 0.
          vg_erro = c_x.
        ELSE.
          READ TABLE it_msg WITH KEY msgtyp = c_s msgnr  = c_311.
          IF sy-subrc NE 0.
            vg_erro = c_x.
          ENDIF.
        ENDIF.

        IF ( vg_erro IS NOT INITIAL ) AND ( doc_aviso_ger IS NOT INITIAL ). "Checar se já tem picking
          SELECT SINGLE vbeln INTO @DATA(lva_vbeln_picking)
            FROM vbfa AS a
           WHERE vbtyp_n EQ 'R'
             AND vbtyp_v EQ '7'
             AND vbelv   EQ @doc_gerados-av_vbeln
             AND NOT EXISTS ( SELECT mblnr FROM mseg AS b WHERE b~smbln = a~vbeln ).

          IF sy-subrc EQ 0.
            CLEAR: vg_erro.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ( vg_erro IS NOT INITIAL ) AND ( gerar_apenas_aviso NE 'X' ) AND ( ( invoice_doc IS INITIAL ) OR
                                         ( NOT gerar_migo IS INITIAL AND NOT invoice_doc IS INITIAL ) ).
      vg_erro = c_x.

      IF ( doc_aviso_ger IS INITIAL  ). "Só estorna se o aviso foi gerado pela própria function.

        REFRESH it_bdcdata.
        CLEAR it_msg.
        PERFORM z_preenche_dbc TABLES it_bdcdata
                                USING: 'X'  'SAPMV50A'   '4104',
       	 	                             ' '  'BDC_OKCODE' '/00',
       	 	                             ' '  'LIKP-VBELN' doc_gerados-av_vbeln.

        PERFORM z_preenche_dbc TABLES it_bdcdata
                                USING: 'X'  'SAPMV50A'   '1000',
       	 	                             ' '  'BDC_OKCODE' '/ELOES_T'.
        CLEAR it_msg.

        "Chama a transação VL32N (estorno)
        CALL TRANSACTION c_vl32n USING it_bdcdata MODE vl_mode UPDATE c_s MESSAGES INTO it_msg.

        LOOP AT it_msg.
          PERFORM add_mensagem TABLES it_out USING wa_mov_estq it_msg c_10 vl_desc_nf.
          IF it_msg-msgtyp EQ c_s.
            CLEAR: doc_gerados-av_vbeln.
          ENDIF.
        ENDLOOP.

      ELSE.
        CLEAR: doc_gerados-av_vbeln.
      ENDIF.

***Confirmar Pedido
    ELSE.

      IF frete_ent_terc IS NOT INITIAL. "Cockpit Frete Entrada ZLES0113

        SELECT * INTO TABLE it_ekes
          FROM ekes
         WHERE ebeln EQ wa_zlest0108-ebeln
           AND ebelp EQ c_00010.

        IF NOT sy-subrc IS INITIAL.
          CLEAR: it_xekes_aux.
          wa_xekes-etens = c_0001. "wa_ekes-etens.
          wa_xekes-ebeln = wa_zlest0108-ebeln.
          wa_xekes-ebelp = c_00010.
          wa_xekes-ebtyp = c_la.
          wa_xekes-erdat = sy-datum.
          wa_xekes-lpein = 1.
          wa_xekes-kzdis = c_x.
          wa_xekes-kz    = c_i.

          APPEND wa_xekes TO it_xekes_aux.

          CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
            EXPORTING
              i_ebeln = wa_zlest0108-ebeln
            TABLES
              xekes   = it_xekes_aux
              yekes   = it_yekes.
        ENDIF.

      ELSE.

        SELECT * INTO TABLE it_ekes
          FROM ekes
         WHERE ebeln EQ wa_mov_estq-po_number
           AND ebelp EQ c_00010.

        IF NOT sy-subrc IS INITIAL.
          CLEAR: it_xekes_aux.
          wa_xekes-etens = c_0001. "wa_ekes-etens.
          wa_xekes-ebeln = wa_mov_estq-po_number.
          wa_xekes-ebelp = c_00010.
          wa_xekes-ebtyp = c_la.
          wa_xekes-erdat = wa_mov_estq-pstng_date.
          wa_xekes-lpein = 1.
          wa_xekes-kzdis = c_x.
          wa_xekes-kz    = c_i.

          APPEND wa_xekes TO it_xekes_aux.

          CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
            EXPORTING
              i_ebeln = wa_mov_estq-po_number
            TABLES
              xekes   = it_xekes_aux
              yekes   = it_yekes.
        ENDIF.

      ENDIF.

      IF ( NOT invoice_doc IS INITIAL ).

        IF NOT doc_gerados-av_vbeln IS INITIAL.
          SELECT SINGLE vbeln mjahr INTO (doc_gerados-mm_mblnr, doc_gerados-mm_mjahr)
            FROM vbfa
           WHERE vbtyp_n EQ 'R'
             AND vbtyp_v EQ '7'
             AND vbelv   EQ doc_gerados-av_vbeln.

          IF sy-subrc IS INITIAL.
            wa_mov_estq_docs-mm_mblnr = doc_gerados-mm_mblnr.
            wa_mov_estq_docs-mm_mjahr = doc_gerados-mm_mjahr.
          ENDIF.
        ENDIF.

        wa_mov_estq_docs-av_vbeln = doc_gerados-av_vbeln.
        MODIFY zmmt_ee_zgr_docs FROM wa_mov_estq_docs.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*        CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*          DESTINATION 'XI_SIGAM_RETURN'
*          TABLES
*            outreturn = it_out.

        DATA: lv_rfc TYPE rfcdest.

        CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

        CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
          EXPORTING
            i_fm          = c_fm
          IMPORTING
            e_rfc         = lv_rfc
          EXCEPTIONS
            no_rfc        = 1
            no_rfc_config = 2
            OTHERS        = 3.

        IF sy-subrc EQ 0.
          CALL FUNCTION c_fm IN BACKGROUND TASK
            DESTINATION lv_rfc
            AS SEPARATE UNIT
            TABLES
              outreturn = it_out.
        ELSE.
          CALL FUNCTION c_fm IN BACKGROUND TASK
            TABLES
              outreturn = it_out.
        ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

        COMMIT WORK.
      ENDIF.

      IF ( frete_ent_terc IS NOT INITIAL ). "Se a chamada for pela ZLES0113

        CLEAR vg_erro.

        wa_zlest0108_rec-vbeln = doc_gerados-av_vbeln.
        MODIFY zlest0108 FROM wa_zlest0108_rec.

        IF sy-subrc NE 0.
          vg_erro = c_x.
        ENDIF.

        wa_zlest0110_rec-vbeln = doc_gerados-av_vbeln.
        MODIFY zlest0110 FROM wa_zlest0110_rec.

        IF sy-subrc NE 0.
          vg_erro = c_x.
        ENDIF.

        LOOP AT it_zlest0109 INTO wa_zlest0109.

          wa_zlest0109-vbeln = doc_gerados-av_vbeln.
          MODIFY zlest0109 FROM wa_zlest0109.

          IF sy-subrc NE 0.
            vg_erro = c_x.
          ENDIF.

        ENDLOOP.

        IF vg_erro IS NOT INITIAL.

          ROLLBACK WORK.

          REFRESH it_bdcdata.
          CLEAR it_msg.
          PERFORM z_preenche_dbc TABLES it_bdcdata
                                  USING: 'X'  'SAPMV50A'   '4104',
         	 	                             ' '  'BDC_OKCODE' '/00',
         	 	                             ' '  'LIKP-VBELN' doc_gerados-av_vbeln.

          PERFORM z_preenche_dbc TABLES it_bdcdata
                                  USING: 'X'  'SAPMV50A'   '1000',
         	 	                             ' '  'BDC_OKCODE' '/ELOES_T'.
          CLEAR it_msg.
          "Chama a transação VL32N p/ Estorno
          CALL TRANSACTION c_vl32n USING it_bdcdata MODE vl_mode UPDATE c_s MESSAGES INTO it_msg.

          LOOP AT it_msg.
            PERFORM add_mensagem TABLES it_out USING wa_mov_estq it_msg c_10 vl_desc_nf.
            IF it_msg-msgtyp EQ c_s.
              CLEAR: doc_gerados-av_vbeln.
            ENDIF.
          ENDLOOP.

        ELSE.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*          CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*            DESTINATION 'XI_SIGAM_RETURN'
*            TABLES
*              outreturn = it_out.

          CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
            EXPORTING
              i_fm          = c_fm
            IMPORTING
              e_rfc         = lv_rfc
            EXCEPTIONS
              no_rfc        = 1
              no_rfc_config = 2
              OTHERS        = 3.

          IF sy-subrc EQ 0.
            CALL FUNCTION c_fm IN BACKGROUND TASK
              DESTINATION lv_rfc
              AS SEPARATE UNIT
              TABLES
                outreturn = it_out.
          ELSE.
            CALL FUNCTION c_fm IN BACKGROUND TASK
              TABLES
                outreturn = it_out.
          ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
          COMMIT WORK.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  IF vg_erro EQ c_x.

    IF frete_ent_terc IS NOT INITIAL. "Cockpit Frete Entrada ZLES0113

      CASE it_msg-msgnr.
        WHEN c_160.
          MESSAGE e160(me) RAISING error.
        WHEN OTHERS.
          MESSAGE e000(z_mm) WITH 'Erro ao tentar gerar Aviso!' RAISING error.
      ENDCASE.

    ELSE.
      MESSAGE e000(z_mm) WITH 'Erro ao tentar gerar Aviso!' RAISING error.
    ENDIF.

  ENDIF.

ENDFUNCTION.
