class ZCL_TRANSPORTATION_UTILS definition
  public
  final
  create public .

public section.

  interfaces ZIF_TRANSPORTATION_UTILS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TRANSPORTATION_UTILS IMPLEMENTATION.


  METHOD zif_transportation_utils~bdc_data.
    CLEAR zif_transportation_utils~w_bdcdata.

    zif_transportation_utils~w_bdcdata-program   = i_prog.
    zif_transportation_utils~w_bdcdata-dynpro    = i_dynpro.
    zif_transportation_utils~w_bdcdata-dynbegin  = i_start.
    zif_transportation_utils~w_bdcdata-fnam      = i_fnam.
    zif_transportation_utils~w_bdcdata-fval      = i_fval.
    APPEND zif_transportation_utils~w_bdcdata   TO zif_transportation_utils~t_bdcdata.
  ENDMETHOD.


  METHOD zif_transportation_utils~criar_log.

    DATA: w_zlest0228 TYPE zlest0228,
          l_timestamp TYPE timestampl,
          l_message   TYPE bdc_vtext1.

*--------------------------
*-- elimina msg duplicadas
*--------------------------
    SORT t_return BY type id number.
    DELETE ADJACENT DUPLICATES FROM t_return
                          COMPARING type id number.

*--------------------------
*-- criar log
*--------------------------
    LOOP AT t_return INTO DATA(w_return).

      DO.
        GET TIME STAMP FIELD l_timestamp.

        SELECT vbeln
          INTO @DATA(l_vbeln)
          FROM zlest0228
            UP TO 1 ROWS
         WHERE vbeln = @i_vbeln
           AND cont  = @l_timestamp.
        ENDSELECT.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDDO.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = w_return-id
          msgnr               = w_return-number
          msgv1               = w_return-message_v1
          msgv2               = w_return-message_v2
          msgv3               = w_return-message_v3
          msgv4               = w_return-message_v4
        IMPORTING
          message_text_output = l_message.

      CLEAR w_zlest0228.
      w_zlest0228-mandt       = sy-mandt.
      w_zlest0228-vbeln       = i_vbeln.
      w_zlest0228-cont        = l_timestamp.
      w_zlest0228-etapa_proc  = i_etapa_proc.
      w_zlest0228-doc_gerado  = i_doc_gerado.
      w_zlest0228-msgtyp      = w_return-type.
      w_zlest0228-msgspra     = sy-langu.
      w_zlest0228-msgid       = w_return-id.
      w_zlest0228-msgnr       = w_return-number.
      w_zlest0228-msgv1       = l_message.
      w_zlest0228-data        = sy-datum.
      w_zlest0228-hora        = sy-uzeit.
      w_zlest0228-usuario     = sy-uname.

      MODIFY zlest0228     FROM w_zlest0228.
    ENDLOOP.

    IF i_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD zif_transportation_utils~eliminar_vt.

    DATA: t_stagedata         TYPE TABLE OF bapishipmentstage,
          t_itemdata          TYPE TABLE OF bapishipmentitem,
          st_itemdata         TYPE bapishipmentitem,
          st_headerdata2      TYPE bapishipmentheader,
          st_headerdataaction TYPE bapishipmentheaderaction,
          t_itemdataaction    TYPE TABLE OF bapishipmentitemaction,
          w_itemdataaction    TYPE bapishipmentitemaction,
          t_return_vt         TYPE TABLE OF bapiret2.

    FREE: st_headerdataaction,  t_itemdataaction, st_headerdata2,
          t_itemdataaction,    t_return_vt,
          t_itemdata.

    CLEAR st_itemdata.
    st_itemdata-delivery   = i_delivery.
    st_itemdata-itenerary  = '0010'.
    APPEND st_itemdata    TO t_itemdata.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_tknum
      IMPORTING
        output = st_headerdata2-shipment_num.

    st_headerdataaction-shipment_num     = 'D'.
    st_headerdataaction-service_agent_id = 'D'.

    LOOP AT t_itemdata      INTO st_itemdata.
      CLEAR: w_itemdataaction.
      MOVE: 'D'               TO w_itemdataaction-delivery,
            'D'               TO w_itemdataaction-itenerary.
      APPEND w_itemdataaction TO t_itemdataaction.
    ENDLOOP.
*

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = st_headerdata2
        headerdataaction = st_headerdataaction
      TABLES
        itemdata         = t_itemdata
        itemdataaction   = t_itemdataaction
        return           = t_return_vt.


    MOVE-CORRESPONDING t_return_vt TO e_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.




  ENDMETHOD.


  METHOD zif_transportation_utils~estornar_doc_custo.

    DATA: l_tp_frete TYPE zde_tp_frete,
          l_fknum    TYPE char40,
          l_vdata    TYPE char40,
          l_mode     TYPE char1,
          t_msg      TYPE TABLE OF bdcmsgcoll,
          w_msg      TYPE bdcmsgcoll,
          t_return   TYPE TABLE OF bapiret2,
          w_return   TYPE bapiret2.

    FREE: l_tp_frete,
          t_return.

    zif_transportation_utils~free_return( ).

    l_fknum = i_fknum.
    l_vdata = sy-datum+6(2) && '.' && sy-datum+4(2) && '.' && sy-datum+0(4).
    l_mode  = 'N'.

    IF i_ktokk IS NOT INITIAL.
      IF i_ktokk = 'ZFIC'.
        l_tp_frete   = 'CIF'.
      ELSE.
        l_tp_frete   = 'CPT'.
      ENDIF.
    ENDIF.

*--------------------------------------
*-- monta tela
*--------------------------------------
    zif_transportation_utils~free_bdc_data( ).

    IF l_tp_frete <> 'CPT'.
      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0020' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKK-FKNUM' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=UEBP' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKK-FKNUM'     i_fval = l_fknum ).

      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0030' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKP-FKPOS(01)' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=PDET' ).

      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKP-POSTX' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=PABR' ).

      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=SICH' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKPD-SLSTOR'   i_fval = 'X' ).
    ELSE.
      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0020' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKK-FKNUM' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=UEBP' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKK-FKNUM'     i_fval = l_fknum ).

      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0030' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKP-FKPOS(01)' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=PDET' ).

      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=PABR' ).

      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=KLAC' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKPD-SLSTOR'   i_fval = 'X' ).

      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '/00' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKPD-STDAT'    i_fval = l_vdata ).

      zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=SICH' ).

    ENDIF.

*--------------------------------------
*-- call vi02
*--------------------------------------
    CALL TRANSACTION 'VI02' USING zif_transportation_utils~t_bdcdata
                MODE l_mode
       MESSAGES INTO t_msg.

*------------------------------------
*-- trata mensagen
*------------------------------------
    LOOP AT t_msg      INTO w_msg.
      w_return-type       = w_msg-msgtyp.
      w_return-id         = w_msg-msgid.
      w_return-number     = w_msg-msgnr.
      w_return-message_v1 = w_msg-msgv1.
      w_return-message_v2 = w_msg-msgv2.
      w_return-message_v3 = w_msg-msgv3.
      w_return-message_v4 = w_msg-msgv4.
      APPEND w_return    TO t_return.
    ENDLOOP.

    MOVE-CORRESPONDING t_return TO e_return.

    READ TABLE t_msg INTO w_msg WITH KEY msgtyp = 'E'.

    CHECK sy-subrc <> 0.

    COMMIT WORK.
    WAIT UP TO 5 SECONDS.

*--------------------------------------
*-- elimina VI
*--------------------------------------
    FREE: t_msg,
          t_return.

    zif_transportation_utils~free_bdc_data( ).

    zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0020' i_start = 'X' i_fnam = ''               i_fval = '' ).
    zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKK-FKNUM' ).
    zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=UEBP' ).
    zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKK-FKNUM'     i_fval = l_fknum ).

    zif_transportation_utils~bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0030' i_start = 'X' i_fnam = ''               i_fval = '' ).
    zif_transportation_utils~bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '/ELOES' ).

*--------------------------------------
*-- call vi02
*--------------------------------------
    CALL TRANSACTION 'VI02' USING zif_transportation_utils~t_bdcdata
                MODE l_mode
       MESSAGES INTO t_msg.

*------------------------------------
*-- trata mensagen
*------------------------------------
    LOOP AT t_msg      INTO w_msg.
      w_return-type       = w_msg-msgtyp.
      w_return-id         = w_msg-msgid.
      w_return-number     = w_msg-msgnr.
      w_return-message_v1 = w_msg-msgv1.
      w_return-message_v2 = w_msg-msgv2.
      w_return-message_v3 = w_msg-msgv3.
      w_return-message_v4 = w_msg-msgv4.
      APPEND w_return    TO t_return.
    ENDLOOP.

    READ TABLE t_msg INTO w_msg WITH KEY msgtyp = 'E'.
    MOVE-CORRESPONDING t_return TO e_return.

    CHECK sy-subrc <> 0.

    COMMIT WORK.
    WAIT UP TO 5 SECONDS.



  ENDMETHOD.


  METHOD zif_transportation_utils~estornar_fatura_servico.

    DATA: t_return2 TYPE TABLE OF bapireturn1,
          t_return  TYPE TABLE OF bapiret2,
          t_success TYPE TABLE OF bapivbrksuccess.

    FREE: t_return,
          t_return2,
          t_success.

*------------------------------------
*-- estorno
*------------------------------------
    CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
      EXPORTING
        billingdocument = i_document
      TABLES
        return          = t_return2
        success         = t_success.

    MOVE-CORRESPONDING t_return2[]  TO t_return[].


    CHECK t_success[] IS NOT INITIAL..

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    WAIT UP TO 5 SECONDS.

  ENDMETHOD.


  METHOD zif_transportation_utils~estornar_ov_servico.

    DATA: tl_bapiparex      TYPE TABLE OF bapiparex,
          sl_bapiparex      TYPE bapiparex,
          wl_orderheaderin  TYPE bapisdh1,
          wl_orderheaderinx TYPE bapisdh1x,
          wl_bape_vbak      TYPE bape_vbak,
          wl_bape_vbakx     TYPE bape_vbakx,
          t_itemdata        TYPE TABLE OF bapishipmentitem,
          st_itemdata       TYPE bapishipmentitem,
          t_return          TYPE TABLE OF bapiret2,
          w_return          TYPE bapiret2.

    zif_transportation_utils~free_return( ).

*--------------------------------------
*-- monta bapi
*--------------------------------------
    FREE: wl_orderheaderin,wl_orderheaderinx,
          tl_bapiparex.

    wl_bape_vbak-vbeln           = i_salesdocument.
    wl_bape_vbak-tknum           = ''.
    sl_bapiparex-structure       = 'BAPE_VBAK'.
    sl_bapiparex-valuepart1      = wl_bape_vbak.
    APPEND sl_bapiparex         TO tl_bapiparex.

    CLEAR sl_bapiparex.
    wl_bape_vbakx-vbeln          = i_salesdocument.
    wl_bape_vbakx-tknum          = 'X'.
    sl_bapiparex-structure       = 'BAPE_VBAKX'.
    sl_bapiparex-valuepart1      = wl_bape_vbakx.
    APPEND sl_bapiparex         TO tl_bapiparex.

    wl_orderheaderin-bill_block  = '10'.
    wl_orderheaderinx-updateflag = 'U'.
    wl_orderheaderinx-bill_block = 'X'.

*--------------------------------------
*-- executa bapi
*--------------------------------------
"*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = i_salesdocument
        order_header_in  = wl_orderheaderin
        order_header_inx = wl_orderheaderinx
      TABLES
        return           = t_return
        extensionin      = tl_bapiparex.


    READ TABLE t_return INTO w_return WITH KEY type = 'E'.

    MOVE-CORRESPONDING t_return TO e_return.

    CHECK sy-subrc <> 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDMETHOD.


  METHOD zif_transportation_utils~finalizar_vt.
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor          |Request   |Data      |Descrição                    &*
*&--------------------------------------------------------------------&*
*& NSEGATIN       |DEVK9A2QBW|21.08.2025|Função Z_LES_VERIFICA_PED_ADM&*
*&                                      |ser chamada qdo. Frete = CIF.&*
*&                                      |Chamado:188635.              &*
*&--------------------------------------------------------------------&*
    DATA: st_headerdata2      TYPE bapishipmentheader,
          st_headerdataaction TYPE bapishipmentheaderaction.


    DATA: t_return   TYPE TABLE OF bapiret2,
          l_tknum    TYPE vttk-tknum,
          l_mensagem TYPE char255.

    DATA: t_return_vt TYPE TABLE OF bapiret2,
          w_return_vt TYPE bapiret2.


    CLEAR : st_headerdataaction, st_headerdata2.
    REFRESH: t_return.

    SELECT SINGLE inco1 INTO @DATA(vl_icon1) FROM likp WHERE vbeln EQ @i_rem_vbeln.    "<<<------"188635 - NMS ------->>>

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_tknum
      IMPORTING
        output = st_headerdata2-shipment_num.

    l_tknum                            = st_headerdata2-shipment_num.
    st_headerdata2-status_load_end     = 'X'.
    st_headerdata2-status_compl        = 'X'.
    st_headerdata2-status_shpmnt_start = 'X'.
    st_headerdata2-status_shpmnt_end   = 'X'.

    st_headerdataaction-status_load_end     = 'C'.
    st_headerdataaction-status_compl        = 'C'.
    st_headerdataaction-status_shpmnt_start = 'C'.
    st_headerdataaction-status_shpmnt_end   = 'C'.


    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = st_headerdata2
        headerdataaction = st_headerdataaction
      TABLES
        return           = t_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
**<<<------"188635 - NMS - INI------>>>
    IF vl_icon1 EQ 'CIF'.
**<<<------"188635 - NMS - FIM------>>>
      CALL FUNCTION 'Z_LES_VERIFICA_PED_ADM'
        EXPORTING
          p_tknum      = st_headerdata2-shipment_num
        EXCEPTIONS
          adiantamento = 1
          pedagio      = 2
          OTHERS       = 3.
**<<<------"188635 - NMS - INI------>>>
    ELSE.
      CLEAR sy-subrc.

    ENDIF.
**<<<------"188635 - NMS - FIM------>>>
    IF sy-subrc IS NOT INITIAL.

      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                          INTO l_mensagem.
      zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
      l_mensagem = 'Documento VT será estornado:' && l_tknum.
      zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).

      t_return = zif_transportation_utils~get_tab_return( ).

      e_return =  t_return.

*-----------------------------
*------ Estorna VT
*-----------------------------
      zif_transportation_utils~eliminar_vt( EXPORTING i_tknum       = l_tknum
                                                      i_delivery    = i_rem_vbeln ).
      FREE: l_tknum.

    ELSE.
      st_headerdata2-status_compl             = 'X'.
      st_headerdata2-status_shpmnt_start      = 'X'.
      st_headerdataaction-status_compl        = 'C'.
      st_headerdataaction-status_shpmnt_start = 'C'.

      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          headerdata       = st_headerdata2
          headerdataaction = st_headerdataaction
        TABLES
          return           = t_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      "Forçar a liberação do documento gerado
      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.
    ENDIF.

    e_doc_transp = l_tknum.

  ENDMETHOD.


  METHOD zif_transportation_utils~free_bdc_data.

    FREE: zif_transportation_utils~t_bdcdata.
  ENDMETHOD.


  METHOD zif_transportation_utils~free_log_proc.
    IF i_etapa_proc IS INITIAL.
      DELETE FROM zlest0228 WHERE vbeln      = i_vbeln
                              AND msgtyp     = zif_transportation_utils~c_e.
    ELSE.
      DELETE FROM zlest0228 WHERE vbeln      = i_vbeln
                              AND etapa_proc = i_etapa_proc.
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD zif_transportation_utils~free_return.
    FREE: zif_transportation_utils~at_t_return[].
  ENDMETHOD.


  METHOD zif_transportation_utils~gerar_doc_custo.

    DATA: l_tknum        TYPE tknum,
          l_mensagem     TYPE char255,
          l_fknum        TYPE zlest0211-fknum,
          l_ov_frete     TYPE zlest0211-ov_frete,
          l_fatura_frete TYPE zlest0211-fatura_frete,
          l_ebeln        TYPE ekko-ebeln,
          l_st_proc      TYPE zst_proc,
          l_dacte        TYPE j_1bdocnum,
          t_return       TYPE TABLE OF bapiret2.

    r_instancia = me.

    FREE: l_fknum,
          l_ov_frete,
          l_st_proc,
          l_fatura_frete,
          e_doc_custo,
          e_ordem_serv,
          e_fatura_serv,
          e_dacte.

    zif_transportation_utils~free_return( ).
*--------------------------------------
*-- tratar nro transporte
*--------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_tknum
      IMPORTING
        output = l_tknum.

*--------------------------------------
*-- emissao documentos
*--------------------------------------
    TRY .
        zcl_faturamento=>zif_faturamento~get_instance(
          )->get_processo_emissao_docs(
            EXPORTING
              i_tknum        = l_tknum
            IMPORTING
              e_doc_custo    = DATA(l_doc_custo)
              e_conhecimento = DATA(l_conhecimento)
              e_tipo_veiculo = DATA(l_tipo_veiculo)
              e_tp_frete     = DATA(l_tp_frete)
              e_manifesto    = DATA(l_manifesto)
          ).

      CATCH zcx_faturamento INTO DATA(ex_faturamento).
        l_mensagem = ex_faturamento->get_longtext( ).
        zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
        e_return = zif_transportation_utils~get_tab_return( ).

        RAISE EXCEPTION TYPE zcx_transportation_utils
          EXPORTING
            textid = VALUE #( msgid  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgid
                              msgno  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgno
                              attr1  = CONV #( i_tknum ) )
            msgty  = 'E'
            msgid  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgid
            msgno  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgno
            msgv1  = CONV #( i_tknum ).


      CATCH zcx_error INTO DATA(ex_error).
        l_mensagem = ex_faturamento->get_longtext( ).
        zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
        e_return = zif_transportation_utils~get_tab_return( ).

        RAISE EXCEPTION TYPE zcx_transportation_utils
          EXPORTING
            textid = VALUE #( msgid  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgid
                              msgno  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgno
                              attr1  = CONV #( i_tknum ) )
            msgty  = 'E'
            msgid  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgid
            msgno  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgno
            msgv1  = CONV #( i_tknum ).
    ENDTRY.

    IF l_tipo_veiculo EQ zif_faturamento=>st_tp_prop_veiculo_proprio.
      DATA(ckfprop) = abap_true.
    ELSE.
      ckfprop = abap_false.
    ENDIF.

*--------------------------------------
*-- local agente frete
*--------------------------------------
    TRY .
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = i_agente_frete
          )->ck_parceiro_local_negocio(
          ).

        DATA(rb_cus) = abap_false.

      CATCH zcx_parceiros.    " .
        IF ckfprop EQ abap_true.
          rb_cus = abap_true.
        ELSEIF ckfprop EQ abap_false AND l_tp_frete EQ zif_carga=>st_tp_frete_cpt.
          rb_cus = abap_true.
        ENDIF.
    ENDTRY.

*--------------------------------------
*-- gerar custo
*--------------------------------------
    SUBMIT zlesr0013 WITH so_tknum = l_tknum
                     WITH p_vbeln  = i_vbeln
                     WITH p_ebeln  = l_ebeln
                     WITH cksetap  = abap_true
                     WITH rb_in    = l_doc_custo    "Documento de custo de frete
                     WITH rb_out   = l_conhecimento "Ordem / Fatura Serviço
                     WITH ckfprop  = ckfprop
                     WITH rb_cus   = rb_cus
                     WITH rb_dtfat = sy-datum
                 AND RETURN.

*--------------------------------------
*-- obter documentos
*--------------------------------------
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD l_fknum.
    GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD l_ov_frete.
    GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD l_fatura_frete.

*--------------------------------------
*-- tratar numero doctos
*--------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_fknum
      IMPORTING
        output = l_fknum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_ov_frete
      IMPORTING
        output = l_ov_frete.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_fatura_frete
      IMPORTING
        output = l_fatura_frete.

*-----------------------------
*-- doc custo
*-----------------------------
    IF l_fknum IS NOT INITIAL.
      l_st_proc  = '05'.
      l_mensagem = 'Gerado Doc.Custo'.
      "zif_transportation_utils~free_return( ).
      zif_transportation_utils~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ELSE.
      l_st_proc  = '04'.
      l_mensagem = 'Não foi gerado Documento Custo.'.
      "zif_transportation_utils~free_return( ).
      zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ENDIF.

*-----------------------------
*-- Ov servico
*-----------------------------
    IF l_ov_frete IS NOT INITIAL.
      l_st_proc  = '06'.
      l_mensagem = 'Gerada Ordem Serviço'.
      "zif_transportation_utils~free_return( ).
      zif_transportation_utils~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ELSE.
      l_mensagem = 'Não foi gerada Ordem de Serviço.'.
      "zif_transportation_utils~free_return( ).
      zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ENDIF.

*-----------------------------
*-- Fatura servico
*-----------------------------
    IF l_fatura_frete IS NOT INITIAL.
      l_st_proc  = '07'.
      l_mensagem = 'Gerada Fatura Serviço'.
      "zif_transportation_utils~free_return( ).
      zif_transportation_utils~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ELSE.
      l_mensagem = 'Não foi gerada Fatura Serviço'.
      "zif_transportation_utils~free_return( ).
      zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ENDIF.

    e_return = zif_transportation_utils~get_tab_return( ).


*-----------------------------
*-- DACTE
*-----------------------------
    IF l_fatura_frete IS NOT INITIAL.
      DO 3 TIMES.
        SELECT j_1bnfdoc~bukrs, j_1bnflin~docnum
          FROM j_1bnflin
         INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
          INTO @DATA(w_doc)
            UP TO 1 ROWS
         WHERE j_1bnflin~refkey EQ @l_fatura_frete.
        ENDSELECT.

        IF sy-subrc <> 0.
          WAIT UP TO 2 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    IF sy-subrc = 0 AND l_fatura_frete IS NOT INITIAL.
      e_dacte = w_doc-docnum.
    ENDIF.

    IF e_dacte IS NOT INITIAL.
      l_st_proc  = '99'.
      l_mensagem = 'Gerada DACTE'.
      "zif_transportation_utils~free_return( ).
      zif_transportation_utils~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ELSE.
      l_mensagem = 'Não foi gerada DACTE'.
      "zif_transportation_utils~free_return( ).
      zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ENDIF.

    e_return = zif_transportation_utils~get_tab_return( ).

*-----------------------------
*-- erro nao gerou doc custo
*-----------------------------
    IF l_fknum IS INITIAL.
      RAISE EXCEPTION TYPE zcx_transportation_utils
        EXPORTING
          textid = VALUE #( msgid  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgid
                            msgno  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgno
                            attr1  = CONV #( i_tknum ) )
          msgty  = 'E'
          msgid  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgid
          msgno  = zcx_transportation_utils=>zcx_erro_criar_doc_custo-msgno
          msgv1  = CONV #( i_tknum ).

    ENDIF.

*-----------------------------
*-- documentos gerados
*-----------------------------
    e_doc_custo   = l_fknum.
    e_ordem_serv  = l_ov_frete.
    e_fatura_serv = l_fatura_frete.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = e_dacte
      IMPORTING
        output = l_dacte.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_on.

    "MOVE-CORRESPONDING t_return TO e_return.


  ENDMETHOD.


  METHOD zif_transportation_utils~gerar_fatura_servico.


  ENDMETHOD.


  METHOD zif_transportation_utils~gerar_ov_servico.


  ENDMETHOD.


  METHOD zif_transportation_utils~gerar_vt.

    DATA: t_return   TYPE TABLE OF bapiret2,
          l_tknum    TYPE vttk-tknum,
          l_mensagem TYPE char255.

*    DATA: st_headerdata2      TYPE bapishipmentheader,
*          st_headerdataaction TYPE bapishipmentheaderaction.

    DATA: t_return_vt TYPE TABLE OF bapiret2,
          w_return_vt TYPE bapiret2.

*-----------------------------------------
*-- Gera o Transporte
*-----------------------------------------
    "zif_transportation_utils~free_return( ).

    DATA: t_stagedata TYPE TABLE OF bapishipmentstage,
          t_itemdata  TYPE TABLE OF bapishipmentitem.

    APPEND i_itemdata  TO t_itemdata .
    APPEND i_stagedata TO t_stagedata.

"*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_SHIPMENT_CREATE'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        headerdata = i_headerdata
      IMPORTING
        transport  = l_tknum
      TABLES
        itemdata   = t_itemdata
        stagedata  = t_stagedata
        return     = t_return_vt.

    IF l_tknum IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      e_return = t_return_vt.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_on.
**-----------------------------
**---- Atualiza documento
**-----------------------------
*      UPDATE zlest0211 SET doc_transp = l_tknum
*                           st_proc    = '04'
*                     WHERE vbeln      = i_rem_vbeln.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = abap_on.
*
*      CLEAR : st_headerdataaction, st_headerdata2.
*      REFRESH: t_return_vt.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = l_tknum
*        IMPORTING
*          output = st_headerdata2-shipment_num.
*
*      l_tknum                            = st_headerdata2-shipment_num.
*      st_headerdata2-status_load_end     = 'X'.
*      st_headerdata2-status_compl        = 'X'.
*      st_headerdata2-status_shpmnt_start = 'X'.
*      st_headerdata2-status_shpmnt_end   = 'X'.
*
*      st_headerdataaction-status_load_end     = 'C'.
*      st_headerdataaction-status_compl        = 'C'.
*      st_headerdataaction-status_shpmnt_start = 'C'.
*      st_headerdataaction-status_shpmnt_end   = 'C'.
*
*
*
*      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
*        EXPORTING
*          headerdata       = st_headerdata2
*          headerdataaction = st_headerdataaction
*        TABLES
*          return           = t_return_vt.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*
*      CALL FUNCTION 'Z_LES_VERIFICA_PED_ADM'
*        EXPORTING
*          p_tknum      = st_headerdata2-shipment_num
*        EXCEPTIONS
*          adiantamento = 1
*          pedagio      = 2
*          OTHERS       = 3.
*
*      IF sy-subrc IS NOT INITIAL.
*
*        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                            INTO l_mensagem.
*        zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
*        l_mensagem = 'Documento VT será estornado:' && l_tknum.
*        zif_transportation_utils~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
*
*        t_return = zif_transportation_utils~get_tab_return( ).
*
**-----------------------------
**------ Estorna VT
**-----------------------------
*        zif_transportation_utils~eliminar_vt( EXPORTING i_tknum       = l_tknum
*                                                        i_delivery    = i_rem_vbeln ).
*        FREE: l_tknum.
*      ELSE.
*        st_headerdata2-status_compl             = 'X'.
*        st_headerdata2-status_shpmnt_start      = 'X'.
*        st_headerdataaction-status_compl        = 'C'.
*        st_headerdataaction-status_shpmnt_start = 'C'.
*
*        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
*          EXPORTING
*            headerdata       = st_headerdata2
*            headerdataaction = st_headerdataaction
*          TABLES
*            return           = t_return_vt.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*        "Forçar a liberação do documento gerado
*        CALL FUNCTION 'DEQUEUE_ALL'
*          EXPORTING
*            _synchron = 'X'.
*      ENDIF.
    ENDIF.

*--------------------------------
*-- documento gerado
*--------------------------------
    e_doc_transp = l_tknum.

  ENDMETHOD.


  METHOD zif_transportation_utils~get_instance.

    IF zif_transportation_utils~at_remessa IS NOT BOUND.
      CREATE OBJECT zif_transportation_utils~at_remessa TYPE zcl_transportation_utils.
      r_instancia = zif_transportation_utils~at_remessa.
    ELSE.
      r_instancia = zif_transportation_utils~at_remessa.
    ENDIF.

  ENDMETHOD.


  METHOD zif_transportation_utils~get_tab_return.

    t_return[] = zif_transportation_utils~at_t_return[].

  ENDMETHOD.


  METHOD zif_transportation_utils~set_tab_return.
    DATA: w_return  TYPE bapiret2.

    w_return-type        = i_type.   "ZIF_TRANSPORTATION_UTILS~c_e.
    w_return-id          = i_id.     "ZIF_TRANSPORTATION_UTILS~c_sd.
    w_return-number      = i_number. "ZIF_TRANSPORTATION_UTILS~c_024.
    w_return-message_v1  = i_message.
    APPEND w_return     TO zif_transportation_utils~at_t_return.

  ENDMETHOD.
ENDCLASS.
