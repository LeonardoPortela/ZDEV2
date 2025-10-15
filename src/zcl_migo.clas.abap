class ZCL_MIGO definition
  public
  final
  create public .

public section.

  constants TP_MVT_IND_ type KZBEW value ' ' ##NO_TEXT.
  constants TP_MVT_IND_B type KZBEW value 'B' ##NO_TEXT.
  constants TP_MVT_IND_F type KZBEW value 'F' ##NO_TEXT.
  constants TP_MVT_IND_L type KZBEW value 'L' ##NO_TEXT.
  constants TP_MVT_IND_K type KZBEW value 'K' ##NO_TEXT.
  constants TP_MVT_IND_O type KZBEW value 'O' ##NO_TEXT.
  constants TP_MVT_IND_W type KZBEW value 'W' ##NO_TEXT.
  constants TP_GOODSMVT_CODE_01 type GM_CODE value '01' ##NO_TEXT.
  constants TP_GOODSMVT_CODE_02 type GM_CODE value '02' ##NO_TEXT.
  constants TP_GOODSMVT_CODE_03 type GM_CODE value '03' ##NO_TEXT.
  constants TP_GOODSMVT_CODE_04 type GM_CODE value '04' ##NO_TEXT.
  constants TP_GOODSMVT_CODE_05 type GM_CODE value '05' ##NO_TEXT.
  constants TP_GOODSMVT_CODE_06 type GM_CODE value '06' ##NO_TEXT.
  constants TP_GOODSMVT_CODE_07 type GM_CODE value '07' ##NO_TEXT.

  methods CRIAR
    importing
      !I_CABECALHO type ZDE_MIGO_CABECALHO
      !I_ITENS type ZDE_MIGO_ITENS_T
      !I_BAPI_WAIT type BAPIWAIT default 'X'
    exporting
      !E_RETORNO type BAPIRET2_T
      !MAT_DOC type MBLNR
      !DOC_YEAR type MJAHR
      !E_J_1BNFDOC type J_1BNFDOC
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_MIGO_EXCEPTION
      ZCX_PEDIDO_COMPRA_EXCEPTION .
  methods ESTORNAR
    importing
      !I_BAPI_WAIT type BAPIWAIT default 'X'
    exporting
      !I_DOC_YEAR_ESTORNO type MJAHR
      !I_MAT_DOC_ESTORNO type MBLNR
      !E_RETORNO type BAPIRET2_T
    changing
      value(I_DOC_YEAR) type MJAHR optional
      value(I_MAT_DOC) type MBLNR optional
      value(I_BUDAT) type BUDAT optional
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_MIGO_EXCEPTION .
  methods SET_NR_DOCUMENTO
    importing
      !I_MBLNR type MBLNR
      !I_MJAHR type MJAHR .
  methods GET_RETORNO
    returning
      value(R_RETORNO) type BAPIRET2_T .
  methods GET_RETORNO_ESTORNO
    returning
      value(R_RETORNO) type BAPIRET2_T .
  class-methods GET_MIGO_VALIDA
    importing
      !I_XBLNR type XBLNR1
      !I_BLDAT type BLDAT
      !I_LE_VBELN type VBELN_VL optional
      !I_LIFNR type ELIFN
    returning
      value(R_MKPF) type MKPF .
  class-methods GET_MIGO_PEDIDO_VALIDA
    importing
      !I_EBELN type EBELN
      !I_XBLNR type XBLNR1
    returning
      value(R_MKPF) type MKPF .
protected section.
private section.

  data AT_MATERIALDOCUMENT type MBLNR .
  data AT_MATDOCUMENTYEAR type MJAHR .
  data AT_RETORNO type BAPIRET2_T .
  data AT_MATERIALDOCUMENT_ESTORNO type MBLNR .
  data AT_MATDOCUMENTYEAR_ESTORNO type MJAHR .
  data AT_RETORNO_ESTORNO type BAPIRET2_T .

  methods LIMPAR .
ENDCLASS.



CLASS ZCL_MIGO IMPLEMENTATION.


  method criar.

    data: movement_header      type bapi2017_gm_head_01,
          movement_items       type bapi2017_gm_item_create_t,
          wa_movement_items    type bapi2017_gm_item_create,
          return               type table of bapiret2,
          lc_gm_code           like i_cabecalho-goodsmvt_code,
          lc_refkey            type j_1brefkey,
          extensionin	         type table of bapiparex,
          wa_expheader         type bapimepoheader, "BAPIMEPOHEADER
          wa_expheaderx        type bapimepoheaderx, "BAPIMEPOHEADERX
          wa_exppoexpimpheader type bapieikp,
          wa_purchaseorder     type bapimepoheader-po_number,
          it_return            type table of bapiret2,
          wa_return            type bapiret2.


    data: v_parametro(10),
          v_bapi(10),
          v_bukrs         type ekko-bukrs,
          v_bsart         type ekko-bsart,
          v_lifnr         type ekko-lifnr,
          v_ktokk         type lfa1-ktokk,
          v_user          type sy-uname.

    me->limpar( ).

    movement_header-bill_of_lading = i_cabecalho-valor_total.
    shift movement_header-bill_of_lading left deleting leading space.
    translate movement_header-bill_of_lading using '.,'.

    if i_cabecalho-ver_gr_gi_slip is not initial.
      movement_header-ver_gr_gi_slipx = abap_true.
    endif.

    movement_header = value #( pstng_date      = i_cabecalho-data_lancamento
                               doc_date        = i_cabecalho-data_documento
                               header_txt      = i_cabecalho-descricao
                               ref_doc_no      = i_cabecalho-doc_referencia
                               bill_of_lading  = movement_header-bill_of_lading
                               ver_gr_gi_slip  = i_cabecalho-ver_gr_gi_slip
                               ver_gr_gi_slipx = movement_header-ver_gr_gi_slipx ).

    lc_gm_code = i_cabecalho-goodsmvt_code.

    if ( i_cabecalho-zchave_nfe is not initial or i_cabecalho-zch_referencia is not initial ) and
       ( i_cabecalho-zid_destinacao is initial ).

    endif.

    if i_cabecalho-zchave_nfe is not initial.
      append value #( structure = 'NFE_FORN' valuepart1 = i_cabecalho-zchave_nfe ) to extensionin.
    endif.

    if i_cabecalho-zch_referencia is not initial.
      append value #( structure = 'ROMANEIO' valuepart1 = i_cabecalho-zch_referencia ) to extensionin.
    endif.

    if i_cabecalho-zid_destinacao is not initial.
      append value #( structure = 'DESTINACAO' valuepart1 = i_cabecalho-zid_destinacao ) to extensionin.
    endif.

    free memory id 'Z110_BAPI'.

    loop at i_itens into data(wa_itens).
*---> 04/07/2023 - Migração S4 - EO
*      wa_movement_items-material   = wa_itens-material.
      data(v_len) = strlen( wa_itens-material ).
      wa_movement_items = value #( base wa_movement_items
                                   material      = cond #( when v_len <= 18
                                                           then wa_itens-material )
                                   material_long = cond #( when v_len > 18
                                                           then wa_itens-material ) ).
*<--- 04/07/2023 - Migração S4 - EO

      wa_movement_items-plant      = wa_itens-local_expedicao.
      wa_movement_items-stge_loc   = wa_itens-deposito.
      wa_movement_items-batch      = wa_itens-lote.
      wa_movement_items-gr_rcpt    = wa_itens-ordem.
      wa_movement_items-item_text  = wa_itens-ordem.
      wa_movement_items-move_type  = wa_itens-tipo_movimento.
      wa_movement_items-entry_qnt  = wa_itens-peso.
      wa_movement_items-vendor     = wa_itens-fornecedor.
      wa_movement_items-po_number  = wa_itens-po_number.
      wa_movement_items-po_item    = wa_itens-po_item.
      wa_movement_items-expirydate = wa_itens-expirydate.
      wa_movement_items-prod_date  = wa_itens-prod_date.
      wa_movement_items-tax_code  = wa_itens-tax_code.
      wa_movement_items-ext_base_amount = wa_itens-ext_base_amont.
      wa_movement_items-move_reas = wa_itens-grund.
      wa_movement_items-unload_pt = wa_itens-unload_pt. "S4 hana RJF
      wa_movement_items-gr_rcpt   = wa_itens-gr_rcpt.   "S4 hana RJF
      wa_movement_items-item_text = wa_itens-item_text. "S4 hana RJF
      "   Movimento de mercadoria sem referência
      "B  Movimento de mercadoria por pedido
      "F  Movimento de mercadoria para a ordem
      "L  Movimento mercadoria por nota de remessa
      "K  Movimento mercadoria por necess.Kanban (só interno WM)
      "O  Alocação posterior de material colocado à disposição
      "W  Ajuste posterior de UM proporcional/produto do material

      if wa_itens-po_number is not initial and wa_itens-po_item is not initial.

        wa_movement_items-mvt_ind = zcl_migo=>tp_mvt_ind_b.
        if i_cabecalho-goodsmvt_code is initial.
          lc_gm_code = zcl_migo=>tp_goodsmvt_code_01.
        endif.

        call method zcl_pedido_compra=>get_chave_controle_conf_item
          exporting
            i_ebeln = wa_itens-po_number
            i_ebelp = wa_itens-po_item
          receiving
            r_bstae = data(r_bstae).

        case r_bstae.
          when '0001'.
            if wa_itens-deliv_numb is initial or wa_itens-deliv_item is initial.
              raise exception type zcx_migo_exception
                exporting
                  textid = value #( msgid = zcx_migo_exception=>zcx_informar_aviso_receb-msgid
                                    msgno = zcx_migo_exception=>zcx_informar_aviso_receb-msgno )
                  msgid  = zcx_migo_exception=>zcx_informar_aviso_receb-msgid
                  msgno  = zcx_migo_exception=>zcx_informar_aviso_receb-msgno.
            else.
              wa_movement_items-deliv_numb = wa_itens-deliv_numb.
              wa_movement_items-deliv_item = wa_itens-deliv_item.
            endif.
        endcase.

      endif.
      wa_movement_items-suppl_vend = wa_itens-suppl_vend.
      append wa_movement_items to movement_items.
      "fixa USD
      select single bukrs, bsart, lifnr
      into (@v_bukrs,@v_bsart, @v_lifnr)
      from ekko
      where ebeln = @wa_movement_items-po_number.

      concatenate v_bukrs v_bsart into v_parametro.
      select count( * )
           from tvarvc
           where name = 'Z_ME21N_INSUMOS'
           and low  = v_parametro.

      if sy-subrc = 0.
        if  i_cabecalho-exch_rate  > 1. "US 78143 - CBRAND
*          select single ktokk from lfa1 into v_ktokk where lifnr = v_lifnr.
*          if v_ktokk ne 'ZFIC'.
            select single waers
              into @data(_ctr_waers)
              from ekko
              where ebeln = @wa_movement_items-po_number.
            if  _ctr_waers = 'USD'.
              clear: wa_purchaseorder, wa_expheader, wa_expheaderx.
              wa_expheader-exch_rate =  i_cabecalho-exch_rate. "US 78143 - CBRAND
              wa_purchaseorder = wa_movement_items-po_number.
              "wa_expheader-ex_rate = zde_nfe_dist_alv-ctr_wkurs.
              wa_expheader-ex_rate_fx = 'X'.

              wa_expheaderx-exch_rate  = 'X'.
              wa_expheaderx-ex_rate_fx = 'X'.


              call function 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
                exporting
                  purchaseorder     = wa_purchaseorder
                  poheader          = wa_expheader
                  poheaderx         = wa_expheaderx
                importing
                  expheader         = wa_expheader
                  exppoexpimpheader = wa_exppoexpimpheader
                tables
                  return            = it_return.

              call function 'BAPI_TRANSACTION_COMMIT'
                exporting
                  wait = i_bapi_wait.

              wait up to 2 seconds.

            endif.
*          endif.
        endif.
      endif.
      "fixa USD
    endloop.

    select single * into @data(wa_t156) from t156 where bwart eq @wa_movement_items-move_type.


    v_bapi = 'GOODS'.
    export v_bapi from v_bapi to memory id 'Z110_BAPI'.

                                                            "US162279-
    v_user = sy-uname.
    data(lc_user_job) = zcl_job=>get_user_job( ).
    if lc_user_job is not initial.
      sy-uname =  lc_user_job.
    endif.
                                                            "US162279-
    call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      exporting
        goodsmvt_header  = movement_header
        goodsmvt_code    = lc_gm_code "01  MB01 - 02 MB31 - 03 MB1A - 04 MB1B - 05 MB1C - 06 MB11 - 07 MB04
      importing
        materialdocument = me->at_materialdocument
        matdocumentyear  = me->at_matdocumentyear
      tables
        goodsmvt_item    = movement_items
        return           = return
        extensionin      = extensionin.

    move return[] to e_retorno.
    move return[] to me->at_retorno.

    delete return where type ne 'E'.

    if return is initial.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = i_bapi_wait.

      mat_doc  = me->at_materialdocument.
      doc_year = me->at_matdocumentyear.
      r_gerou  = abap_true.

      if wa_t156-j_1bnftype is not initial.

        concatenate me->at_materialdocument me->at_matdocumentyear into lc_refkey.

        data(i) = 0.
        while i <= 5 and e_j_1bnfdoc is initial.

          wait up to 1 seconds.

          select single * into @data(wa_j_1bnflin)
            from j_1bnflin
           where refkey eq @lc_refkey
             and reftyp eq 'MD'.

          if sy-subrc is initial.
            select single * into e_j_1bnfdoc
              from j_1bnfdoc
             where docnum eq wa_j_1bnflin-docnum.
          endif.
          add 1 to i.

        endwhile.

      endif.

      message s001 with me->at_materialdocument me->at_matdocumentyear.
    else.
      r_gerou  = abap_false.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
    endif.
    free memory id 'Z110_BAPI'.
    sy-uname = v_user.
  endmethod.


  method estornar.

    data: goodsmvt_pstng_date type bapi2017_gm_head_02-pstng_date,
          return              type table of bapiret2,
          document_estorno    type bapi2017_gm_head_ret,
          v_user              type sy-uname.

    if i_mat_doc is initial and i_doc_year is initial.
      i_mat_doc  = me->at_materialdocument.
      i_doc_year = me->at_matdocumentyear.
    endif.

    "Verificar se já está estornado """"""""""""""""""""""""""""""""""

    select * into table @data(it_mseg)
      from mseg as m
     where m~mblnr eq @i_mat_doc
       and m~mjahr eq @i_doc_year
       and m~sjahr eq @space
       and not exists ( select * from mseg as m1 where m1~sjahr eq m~mjahr and m1~smbln eq m~mblnr and m1~smblp eq m~zeile ).

    describe table it_mseg lines data(lc_qtd_linhas).

    if lc_qtd_linhas is initial.

      raise exception type zcx_migo_exception
        exporting
          textid = value #( msgid = zcx_migo_exception=>zcx_estornado-msgid
                            msgno = zcx_migo_exception=>zcx_estornado-msgno
                            attr1 = conv #( i_mat_doc )
                            attr2 = conv #( i_doc_year ) )
          msgid  = zcx_migo_exception=>zcx_estornado-msgid
          msgno  = zcx_migo_exception=>zcx_estornado-msgno
          msgty  = 'E'
          msgv1  = conv #( i_mat_doc )
          msgv2  = conv #( i_doc_year ).

    endif.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "US162279-
    v_user = sy-uname.
    data(lc_user_job) = zcl_job=>get_user_job( ).
    if lc_user_job is not initial.
      sy-uname =  lc_user_job.
    endif.
    "US162279-

    goodsmvt_pstng_date = i_budat.

    call function 'BAPI_GOODSMVT_CANCEL'
      exporting
        materialdocument    = i_mat_doc
        matdocumentyear     = i_doc_year
        goodsmvt_pstng_date = goodsmvt_pstng_date
      importing
        goodsmvt_headret    = document_estorno
      tables
        return              = return.

    i_doc_year_estorno = document_estorno-doc_year.
    i_mat_doc_estorno  = document_estorno-mat_doc.

    me->at_matdocumentyear_estorno  = document_estorno-doc_year.
    me->at_materialdocument_estorno = document_estorno-mat_doc.

    move return[] to e_retorno.
    move return[] to me->at_retorno_estorno.

    delete return where type <> 'E'.

    if return is initial.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = i_bapi_wait.

      r_gerou  = abap_true.
      "select single * from ZPPT0016 into @data(wa_ZPPT0016) where MBLNR eq @I_MAT_DOC. "ZPP0014 Edição lotes criados a partir da ZMM0110 - BG #138699
      delete from zppt0016 where mblnr eq @i_mat_doc. "ZPP0014 Edição lotes criados a partir da ZMM0110 - BG #138699

      message s002 with i_mat_doc i_doc_year i_mat_doc_estorno i_doc_year_estorno .
    else.
      r_gerou  = abap_false.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
    endif.

    sy-uname = v_user.

  endmethod.


  METHOD GET_MIGO_PEDIDO_VALIDA.

    DATA: IT_EKBE TYPE TABLE OF EKBE.

    CLEAR: R_MKPF, IT_EKBE[].

*----------------------------------------------------------------------------------*
*   Migos geradas sem Aviso
*----------------------------------------------------------------------------------*
    SELECT *
      FROM EKBE AS M INTO TABLE IT_EKBE
     WHERE EBELN EQ I_EBELN
       AND XBLNR EQ I_XBLNR
       AND VGABE EQ '1'  "Entrada de mercadorias

       AND EXISTS ( SELECT MBLNR
                      FROM MSEG AS E
                     WHERE E~MBLNR = M~BELNR
                       AND E~MJAHR = M~GJAHR
                       AND E~SMBLN = ' ' "Não pode ser Documento de Estorno
                  )

       "Desconsiderar caso o documento esteja estornado
       AND NOT EXISTS ( SELECT MBLNR
                          FROM MSEG AS E
                         WHERE E~SMBLN = M~BELNR
                           AND E~SJAHR = M~GJAHR ).


*----------------------------------------------------------------------------------*
*   Migos geradas por Aviso
*----------------------------------------------------------------------------------*

    SELECT *
      FROM EKBE AS M APPENDING TABLE IT_EKBE
     WHERE EBELN EQ I_EBELN
       AND VGABE EQ '1'  "Entrada de mercadorias

       "Gerada por aviso
       AND EXISTS ( SELECT VBELN
                      FROM LIKP AS A
                     WHERE A~VBELN EQ M~XBLNR
                       AND A~XBLNR EQ I_XBLNR )

       AND EXISTS ( SELECT MBLNR
                      FROM MSEG AS E
                     WHERE E~MBLNR = M~BELNR
                       AND E~MJAHR = M~GJAHR
                       AND E~SMBLN = ' ' "Não pode ser Documento de Estorno
                   )

       "Desconsiderar caso o documento esteja estornado
       AND NOT EXISTS ( SELECT MBLNR
                          FROM MSEG AS E
                         WHERE E~SMBLN = M~BELNR
                           AND E~SJAHR = M~GJAHR ).

    LOOP AT IT_EKBE INTO DATA(WL_EKBE).

      SELECT SINGLE * INTO R_MKPF
        FROM MKPF
       WHERE MBLNR EQ WL_EKBE-BELNR
         AND MJAHR EQ WL_EKBE-GJAHR.

      IF SY-SUBRC EQ 0.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_MIGO_VALIDA.

    DATA: IT_MIGO TYPE TABLE OF MKPF.

    CLEAR: R_MKPF.

    IF I_LE_VBELN IS NOT INITIAL.
      SELECT SINGLE * INTO @R_MKPF
        FROM MKPF AS M
       WHERE LE_VBELN EQ @I_LE_VBELN
         AND TCODE2   EQ 'VL32N'
         AND BLDAT    GE @I_BLDAT
         AND EXISTS ( SELECT * FROM MSEG AS E WHERE E~MBLNR EQ M~MBLNR AND E~MJAHR EQ M~MJAHR AND E~LIFNR EQ @I_LIFNR AND E~SMBLN EQ @SPACE )
         AND NOT EXISTS ( SELECT * FROM MSEG AS E WHERE E~SMBLN EQ M~MBLNR AND E~SJAHR EQ M~MJAHR ).
    ELSE.

      TRY.
          EXEC SQL.
            OPEN SQL_MIGO FOR
              SELECT M.MBLNR, M.MJAHR
                FROM SAPHANADB.MKPF M
               WHERE M.MANDT = :SY-MANDT
                 AND M.XBLNR = :I_XBLNR
                 AND M.BLDAT >= :I_BLDAT
                 AND EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.MBLNR = M.MBLNR AND E.MJAHR = M.MJAHR AND E.LIFNR = :I_LIFNR AND E.SMBLN = ' ' )
                 AND NOT EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.SMBLN = M.MBLNR AND E.SJAHR = M.MJAHR )
          ENDEXEC.
        CATCH CX_SY_NATIVE_SQL_ERROR INTO DATA(EXC_REF).
          DATA(ERROR_TEXT) = EXC_REF->GET_TEXT( ).
          MESSAGE ERROR_TEXT TYPE 'E'.
      ENDTRY.

      DO.
        EXEC SQL.
          FETCH NEXT SQL_MIGO INTO
          :R_MKPF-MBLNR,
          :R_MKPF-MJAHR
        ENDEXEC.
        IF SY-SUBRC <> 0.
          EXIT.
        ELSE.
          APPEND R_MKPF TO IT_MIGO.
        ENDIF.
      ENDDO.

      EXEC SQL.
        CLOSE SQL_MIGO
      ENDEXEC.

      READ TABLE IT_MIGO INTO R_MKPF INDEX 1.
      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE * INTO R_MKPF
          FROM MKPF
         WHERE MBLNR EQ R_MKPF-MBLNR
           AND MJAHR EQ R_MKPF-MJAHR.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GET_RETORNO.
    R_RETORNO = ME->AT_RETORNO.
  ENDMETHOD.


  METHOD GET_RETORNO_ESTORNO.
    R_RETORNO = ME->AT_RETORNO_ESTORNO.
  ENDMETHOD.


  METHOD LIMPAR.

    CLEAR: ME->AT_MATDOCUMENTYEAR,
           ME->AT_MATERIALDOCUMENT,
           ME->AT_RETORNO,
           ME->AT_MATDOCUMENTYEAR_ESTORNO,
           ME->AT_MATERIALDOCUMENT_ESTORNO,
           ME->AT_RETORNO_ESTORNO.

  ENDMETHOD.


  METHOD SET_NR_DOCUMENTO.
    ME->AT_MATERIALDOCUMENT = I_MBLNR.
    ME->AT_MATDOCUMENTYEAR  = I_MJAHR.
  ENDMETHOD.
ENDCLASS.
