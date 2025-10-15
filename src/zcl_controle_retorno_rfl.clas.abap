class ZCL_CONTROLE_RETORNO_RFL definition
  public
  final
  create public .

public section.

  interfaces ZIF_CONTROLE_RETORNO_RFL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CONTROLE_RETORNO_RFL IMPLEMENTATION.


  METHOD zif_controle_retorno_rfl~cancelar_retorno.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZIF_CONTROLE_RETORNO_RFL~CANCELAR_RETORNO                                            *
*& Chamado        : USER STORY 157683                                                                    *
*& Data           : 12/11/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 12/11/2024|DEVK9A1XAW |NSEGATIN       | Verificação de impedimento de cancelamento do retorno         *
*--------------------------------------------------------------------------------------------------------*
**<<<------"157683 - NMS - INI------>>>
*    DATA: tl_export    TYPE TABLE OF zsdt_export.
*
*    DATA: vl_data_val      TYPE sy-datum,
*          v_docnum_estorno TYPE j_1bnfdoc-docnum.
*
*    DATA: vl_docsta      TYPE j_1bnfe_active-docsta,
*          vl_scssta      TYPE j_1bnfe_active-scssta,
*          lv_diferente   TYPE c,
*          lr_finalidades TYPE RANGE OF zsdt0359-finalidade.
*
*    CLEAR: vl_data_val, v_docnum_estorno, tl_export[].
*
**** Inicio -  Rubenilson - 09.10.24 #154153
*    SELECT *
*      FROM zsdt0359
*      INTO TABLE @DATA(lt_ZSDT0359).
*    IF sy-subrc IS INITIAL.
*      lr_finalidades = VALUE #( FOR ls_zsdt0359 IN lt_zsdt0359
*                                ( sign   = 'I'
*                                  option = 'EQ'
*                                  low    = ls_zsdt0359-finalidade )
*                                  ).
*    ENDIF.
**** Fim -  Rubenilson - 09.10.24 #154153
*
*    SELECT SINGLE *
*      FROM j_1bnfdoc INTO @DATA(wl_doc_retorno)
*     WHERE docnum EQ @i_docnum.
*
*    IF ( sy-subrc NE 0 ) OR ( i_docnum IS INITIAL ).
*      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
*        EXPORTING
*          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgid
*                            msgno = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgno
*                            attr1 = CONV #( i_docnum )
*                            )
*          msgty  = 'E'
*          msgno  = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgno
*          msgid  = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgid
*          msgv1  = CONV #( i_docnum ).
*    ENDIF.
*
*
****/// [CS2020000805] - Ajustar melhorias da transação ZSDT0163 - Set/2021 - Inicio
*
*    CLEAR: vl_docsta,vl_scssta.
*
*    SELECT SINGLE nfenum
*      INTO @DATA(vl_nfenum)
*      FROM j_1bnfdoc
*      WHERE docnum = @i_docnum.
*
*    SELECT SINGLE docsta scssta
*      INTO (vl_docsta,vl_scssta)
*      FROM j_1bnfe_active
*      WHERE docnum = i_docnum.
*
*    IF vl_nfenum IS NOT INITIAL OR ( ( vl_docsta = '1' AND vl_docsta = '2' OR vl_docsta = '2' ) AND ( vl_docsta = '4' ) ).
*      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
*        EXPORTING
*          textid = VALUE #( msgid = 'SD'
*                            msgno = '836'
*                            attr1 = CONV #( i_docnum )
*                            )
*          msgty  = 'E'
*          msgno  = '836'
*          msgid  = 'SD'
*          msgv1  = 'Não permitido estorno'
*          msgv2  = 'do documento com'
*          msgv3  = 'o status atual'
*          msgv4  = CONV #( i_docnum ).
*
*      MESSAGE i836(sd) DISPLAY LIKE 'E' WITH 'Não permitido estorno' 'do documento com' 'o status atual'.
*    ENDIF.
*
*
****/// [CS2020000805] - Ajustar melhorias da transação ZSDT0163 - Set/2021 - Fim
*
*
*    SELECT SINGLE *
*      FROM zsdt_retlote INTO @DATA(wl_retlote)
*     WHERE docnum_ret = @i_docnum.
*
*    IF ( sy-subrc NE 0 ) OR ( i_docnum IS INITIAL ).
*      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
*        EXPORTING
*          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_documento_ret_not_found-msgid
*                            msgno = zcx_controle_retorno_rfl=>zcx_documento_ret_not_found-msgno
*                            attr1 = CONV #( i_docnum )
*                            )
*          msgty  = 'E'
*          msgno  = zcx_controle_retorno_rfl=>zcx_documento_ret_not_found-msgno
*          msgid  = zcx_controle_retorno_rfl=>zcx_documento_ret_not_found-msgid
*          msgv1  = CONV #( i_docnum ).
*    ENDIF.
*
*    SELECT SINGLE *
*      FROM zfiwrt0008 INTO @DATA(wl_zfiwrt008)
*     WHERE docret_quebra   EQ @i_docnum
*       AND docs_estornados EQ @abap_false
*       AND loekz           EQ @abap_false.
*
*    IF sy-subrc EQ 0.
*      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
*        EXPORTING
*          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_exists_retorno_quebra-msgid
*                            msgno = zcx_controle_retorno_rfl=>zcx_exists_retorno_quebra-msgno
*                            attr1 = CONV #( wl_zfiwrt008-seq_lcto )
*                            attr2 = CONV #( i_docnum )
*                            )
*          msgty  = 'E'
*          msgno  = zcx_controle_retorno_rfl=>zcx_exists_retorno_quebra-msgno
*          msgid  = zcx_controle_retorno_rfl=>zcx_exists_retorno_quebra-msgid
*          msgv1  = CONV #( wl_zfiwrt008-seq_lcto )
*          msgv2  = CONV #( i_docnum ).
*    ENDIF.
*
*
*    CALL FUNCTION 'Z_RET_DATA_MES_ABERTO'
*      EXPORTING
*        p_data_ent  = wl_doc_retorno-pstdat
*        p_bukrs     = wl_doc_retorno-bukrs
*      IMPORTING
*        p_data_val  = vl_data_val
*      EXCEPTIONS
*        sem_periodo = 1
*        OTHERS      = 2.
*
*    IF vl_data_val+0(6) NE wl_doc_retorno-pstdat+0(6) AND wl_doc_retorno-pstdat+4(2) LE 12.
*      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
*        EXPORTING
*          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_periodo_fi_fechado-msgid
*                            msgno = zcx_controle_retorno_rfl=>zcx_periodo_fi_fechado-msgno )
*          msgty  = 'E'
*          msgno  = zcx_controle_retorno_rfl=>zcx_periodo_fi_fechado-msgno
*          msgid  = zcx_controle_retorno_rfl=>zcx_periodo_fi_fechado-msgid.
*    ENDIF.
*
*    SELECT *
*      FROM zsdt_export INTO CORRESPONDING FIELDS OF TABLE tl_export
*     WHERE docnum = i_docnum.
*
*    LOOP AT tl_export INTO DATA(wl_export).
*      IF ( wl_export-ordem  IS NOT INITIAL ) OR
*         ( wl_export-export IS NOT INITIAL ).
*        RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
*          EXPORTING
*            textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_doc_ret_exists_exp-msgid
*                              msgno = zcx_controle_retorno_rfl=>zcx_doc_ret_exists_exp-msgno
*                              attr1 = CONV #( i_docnum )
*                              )
*            msgty  = 'E'
*            msgno  = zcx_controle_retorno_rfl=>zcx_doc_ret_exists_exp-msgno
*            msgid  = zcx_controle_retorno_rfl=>zcx_doc_ret_exists_exp-msgid
*            msgv1  = CONV #( i_docnum ).
*      ENDIF.
*    ENDLOOP.
*
**** Inicio - Rubenilson Pereira - 06.08.24 - US146169
*    READ TABLE tl_export ASSIGNING FIELD-SYMBOL(<fs_export>) INDEX 1.
*    IF sy-subrc IS INITIAL.
*      IF <fs_export>-finalidade IN lr_finalidades. " Rubenilson - 09.10.24 #154153
*        SELECT *
*          FROM zsdt_retlote
*          INTO TABLE @DATA(lt_retlote)
*          WHERE docnum_Ret = @<fs_export>-docnum.
*        IF sy-subrc IS INITIAL.
*
*          DATA(lt_retlote_aux) = lt_retlote.
*          SORT lt_retlote_aux BY docnum.
*          DELETE ADJACENT DUPLICATES FROM lt_retlote_aux COMPARING docnum.
*
*          SELECT *
*            FROM zsdtflote_flote
*            INTO TABLE @DATA(lt_flote)
*            FOR ALL ENTRIES IN @lt_retlote_aux
*            WHERE docnum = @lt_retlote_aux-docnum.
*          IF sy-subrc IS INITIAL.
*
*            DATA(lt_flote_aux) = lt_flote.
*            SORT lt_flote_aux BY docnum.
*            DELETE ADJACENT DUPLICATES FROM lt_flote_aux COMPARING docnum.
*
*            SELECT docnum,itmnum,menge
*              FROM j_1bnflin
*              INTO TABLE @DATA(lt_lin)
*              FOR ALL ENTRIES IN @lt_flote_aux
*              WHERE docnum = @lt_flote_aux-docnum.
*            IF sy-subrc IS INITIAL.
*              SORT lt_lin BY docnum itmnum.
*
*              LOOP AT lt_flote ASSIGNING FIELD-SYMBOL(<fs_flote>).
*
*                READ TABLE lt_lin ASSIGNING FIELD-SYMBOL(<fs_lin>)
*                WITH KEY docnum = <fs_flote>-docnum
*                         itmnum = <fs_flote>-itmnum
*                BINARY SEARCH.
*                IF sy-subrc IS INITIAL.
*                  IF <fs_flote>-saldo_disponivel <> <fs_lin>-menge.
*                    lv_diferente = abap_true.
*                    EXIT.
*                  ENDIF.
*                ENDIF.
*
*              ENDLOOP.
*
*              IF lv_diferente IS NOT INITIAL.
*
*                RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
*                  EXPORTING
*                    textid = VALUE #( msgid = 'ZSD'
*                                      msgno = '000'
*                                      attr1 = TEXT-e01
*                                      attr2 = TEXT-e02
*                                      attr3 = TEXT-e03
*                                      )
*                    msgty  = 'E'
*                    msgno  = '000'
*                    msgid  = 'ZSD'
*                    msgv1  = TEXT-e01
*                    msgv2  = TEXT-e02
*                    msgv3  = TEXT-e03.
*
*              ENDIF.
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
**** Fim - Rubenilson Pereira - 06.08.24 - US146169
*
*    LOOP AT tl_export INTO wl_export WHERE ordem  IS INITIAL AND
*                                           export IS INITIAL.
*      DELETE zsdt_export FROM wl_export.
*    ENDLOOP.
*
*    DELETE FROM zsdt_retlote WHERE docnum_ret = i_docnum.
*
*    UPDATE zsdtflote_flote SET cancel = abap_true
*                               us_cancel = sy-uname
*                               dt_cancel = sy-datum
*                               hr_cancel = sy-uzeit
*                         WHERE docnum = i_docnum.
*
*    SELECT COUNT( DISTINCT docnum_rt )
*      FROM znom_remetente INTO @DATA(lv_cont_memo)
*     WHERE docnum_rt = @i_docnum.
*
*    IF lv_cont_memo IS NOT INITIAL.
*      UPDATE znom_remetente SET docnum_rt = '' WHERE docnum_rt = i_docnum.
*    ENDIF.
*
*    SELECT SINGLE *
*      FROM zsdt0053 INTO @DATA(_wl_0053)
*     WHERE docnum_rt EQ @i_docnum.
*
*    IF sy-subrc EQ 0.
*      UPDATE zsdt0053 SET docnum_rt = '0000000000' WHERE docnum_rt = i_docnum.
*    ENDIF.
    DATA: v_docnum_estorno TYPE j_1bnfdoc-docnum.

    TRY.
* Verificação de impedimento de cancelamento do retorno.
    zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->validar_cancelamento_retorno( EXPORTING i_docnum = i_docnum
                                                                                                      IMPORTING e_erro = DATA(vl_erro) ).
    CATCH zcx_controle_retorno_rfl INTO DATA(zcxl_controle_rfl).
      zcxl_controle_rfl->published_erro( EXPORTING i_msgty = 'E' i_msgty_display = 'W' ).
      DATA(lv_text) = zcxl_controle_rfl->get_text( ).
      MESSAGE lv_text TYPE 'E'.
      RETURN.

    ENDTRY.
* Verifica se a validação deu erro.
    CHECK vl_erro IS INITIAL.
**<<<------"157683 - NMS - FIM------>>>
    CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
      EXPORTING
        doc_number               = i_docnum
        ref_type                 = space
        ref_key                  = space
        can_dat                  = sy-datum
      IMPORTING
        doc_number               = v_docnum_estorno
      EXCEPTIONS
        document_not_found       = 1
        cancel_not_possible      = 2
        nf_cancel_type_not_found = 3
        database_problem         = 4
        docum_lock               = 5
        nfe_cancel_simulation    = 6
        OTHERS                   = 7.

    IF sy-subrc EQ 0.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      r_docnum_estorno = v_docnum_estorno.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.


  ENDMETHOD.


  method ZIF_CONTROLE_RETORNO_RFL~DETERMINAR_CFOP.

  DATA: VL_REGIO   TYPE KNA1-REGIO,
        VL_REGION  TYPE ADRC-REGION,
        VL_ADRNR   TYPE J_1BBRANCH-ADRNR,
        VL_DESTINO TYPE ZSDT_RETCFOP-DESTINO.

  CLEAR: E_CFOP, E_LIFNR, VL_REGIO, VL_REGION, VL_ADRNR, VL_DESTINO.

  SELECT SINGLE LIFNR
    FROM VBPA
    INTO E_LIFNR
  WHERE  VBELN EQ I_REFKEY
    AND  PARVW EQ 'Z1'.

  SELECT SINGLE REGIO
    FROM LFA1
    INTO VL_REGIO
  WHERE  LIFNR EQ E_LIFNR.

  SELECT SINGLE ADRNR
    FROM J_1BBRANCH
    INTO VL_ADRNR
  WHERE  BUKRS  EQ I_BUKRS
    AND  BRANCH EQ I_BRANCH.

  SELECT SINGLE REGION
    FROM ADRC
    INTO VL_REGION
  WHERE  ADDRNUMBER EQ VL_ADRNR.

  IF VL_REGIO NE VL_REGION.
    VL_DESTINO = SPACE.
  ELSE.
    VL_DESTINO = 'X'.
  ENDIF.

  SELECT SINGLE CFOP
    FROM ZSDT_RETCFOP
    INTO E_CFOP
  WHERE  MATNR   EQ I_MATNR
    AND  DESTINO EQ VL_DESTINO.

  endmethod.


  METHOD zif_controle_retorno_rfl~gerar_retorno.

    DATA: sl_header     TYPE bapi_j_1bnfdoc,
          sl_header_add TYPE bapi_j_1bnfdoc_add,
          sl_item       TYPE bapi_j_1bnflin,
          sl_item_add   TYPE bapi_j_1bnflin_add,
          sl_export     TYPE zsdt_export,
          sl_ret        TYPE zsdt_retlote,
          sl_partner    TYPE bapi_j_1bnfnad,
          sl_nfcheck    TYPE bapi_j_1bnfcheck.

    DATA: tl_partner  TYPE TABLE OF bapi_j_1bnfnad,
          tl_item     TYPE TABLE OF bapi_j_1bnflin,
          tl_item_add TYPE TABLE OF bapi_j_1bnflin_add,
          tl_item_tax TYPE TABLE OF bapi_j_1bnfstx,
          tl_return   TYPE TABLE OF bapiret2,
          tl_ret      TYPE TABLE OF zsdt_retlote,
          tl_msg      TYPE TABLE OF bapi_j_1bnfftx.

    DATA: vl_itmnum         TYPE j_1bnflin-itmnum,
          vl_cfop           TYPE j_1bcfop,
          vl_lifnr          TYPE vbpa-lifnr,
          vl_netwr_retorno  TYPE j_1bnflin-netwr,
          vl_menge_retorno  TYPE j_1bnflin-menge,
          vl_docnum_retorno TYPE j_1bnfdoc-docnum,
          vl_prazo          TYPE ze_prazo.

    CLEAR: vl_docnum_retorno.

*----------------------------------------------------------------------------------*
* Validar Retorno
*----------------------------------------------------------------------------------*

    me->zif_controle_retorno_rfl~valida_retorno( EXPORTING i_check_exists_ret_finalidade = i_check_exists_ret_finalidade
                                                           i_finalidade                  = i_finalidade
                                                 IMPORTING e_menge_retorno = vl_menge_retorno
                                                           e_netwr_retorno = vl_netwr_retorno ).

*----------------------------------------------------------------------------------*
* Selecão dados
*----------------------------------------------------------------------------------*

    LOOP AT me->zif_controle_retorno_rfl~at_notas_selecionadas INTO DATA(wl_nota_sel) WHERE check_sel EQ abap_true.
      EXIT.
    ENDLOOP.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(wl_doc)
     WHERE docnum EQ @wl_nota_sel-docnum.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(wl_lin)
     WHERE docnum EQ @wl_nota_sel-docnum.

    DATA(_nf_type) = 'ZV'.

    SELECT SINGLE * INTO @DATA(wa_j_1baa)
      FROM j_1baa
     WHERE nftype EQ @_nf_type.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_categoria_nf_not_found-msgid
                            msgno = zcx_controle_retorno_rfl=>zcx_categoria_nf_not_found-msgno
                            attr1 = CONV #( _nf_type )
                            )
          msgty  = 'E'
          msgno  = zcx_controle_retorno_rfl=>zcx_categoria_nf_not_found-msgno
          msgid  = zcx_controle_retorno_rfl=>zcx_categoria_nf_not_found-msgid
          msgv1  = CONV #( _nf_type ).
    ENDIF.

    IF ( wa_j_1baa-form IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_categoria_sem_form-msgid
                            msgno = zcx_controle_retorno_rfl=>zcx_categoria_sem_form-msgno
                            attr1 = CONV #( _nf_type )
                            )
          msgty  = 'E'
          msgno  = zcx_controle_retorno_rfl=>zcx_categoria_sem_form-msgno
          msgid  = zcx_controle_retorno_rfl=>zcx_categoria_sem_form-msgid
          msgv1  = CONV #( _nf_type ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_j_1bb2)
      FROM j_1bb2
     WHERE bukrs  EQ @wl_doc-bukrs
       AND branch EQ @wl_doc-branch
       AND form   EQ @wa_j_1baa-form.

*----------------------------------------------------------------------------------*
* Preenchimento Header
*----------------------------------------------------------------------------------*

    MOVE-CORRESPONDING wa_j_1baa TO sl_header.

    sl_header-docdat  = i_dt_retorno.
    sl_header-pstdat  = sy-datum.
    sl_header-credat  = sy-datum.
    sl_header-manual  = 'X'.
    sl_header-waerk   = 'BRL'.
    sl_header-bukrs   = wl_doc-bukrs.
    sl_header-branch  = wl_doc-branch.
    sl_header-parvw   = 'AG'.
    sl_header-parid   = wl_doc-parid.
    sl_header-partyp  = 'C'.
    sl_header-series  = wa_j_1bb2-series.
    CONDENSE sl_header-series.

*----------------------------------------------------------------------------------*
* Preenchimento Header ADD
*----------------------------------------------------------------------------------*

    sl_header_add-nftot = vl_netwr_retorno.

*----------------------------------------------------------------------------------*
* Preenchimento NF Check
*----------------------------------------------------------------------------------*

    sl_nfcheck-chekcon  = 'X'.

*----------------------------------------------------------------------------------*
* Determinar CFOP
*----------------------------------------------------------------------------------*

    me->zif_controle_retorno_rfl~determinar_cfop(
      EXPORTING
        i_matnr  =   me->zif_controle_retorno_rfl~at_matnr
        i_bukrs  =   wl_doc-bukrs
        i_branch =   wl_doc-branch
        i_kunnr  =   wl_doc-parid
        i_refkey =   wl_lin-refkey
      IMPORTING
        e_cfop   =   vl_cfop
        e_lifnr  =   vl_lifnr  ).

*----------------------------------------------------------------------------------*
* Preenche Partners
*----------------------------------------------------------------------------------*

    sl_partner-parvw  = 'AG'.
    sl_partner-parid  = me->zif_controle_retorno_rfl~at_kunnr.
    sl_partner-partyp = 'C'.
    APPEND sl_partner TO tl_partner.

    sl_partner-parvw  = 'Z1'.
*** CSTASK0013723 - RC - Início
*    sl_partner-parid  = me->zif_controle_retorno_rfl~at_terminal.
    IF sy-tcode = 'ZSDT0163'.

      sl_partner-parid  = i_parceiro.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = sl_partner-parid
        IMPORTING
          output = sl_partner-parid.


    ELSE.

      sl_partner-parid  = me->zif_controle_retorno_rfl~at_terminal.

    ENDIF.
*** CSTASK0013723 - RC - Fim
    sl_partner-partyp = 'V'.
    APPEND sl_partner TO tl_partner.

    IF vl_lifnr IS NOT INITIAL.
      sl_partner-parvw  = 'LF'.
      sl_partner-parid  = vl_lifnr.
      sl_partner-partyp = 'V'.
      APPEND sl_partner TO tl_partner.
    ENDIF.

*----------------------------------------------------------------------------------*
* Preenche Item
*----------------------------------------------------------------------------------*

    ADD 10 TO vl_itmnum.

    sl_item-itmnum  = vl_itmnum.
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
*  SL_ITEM-MATNR   = WL_LIN-MATNR.
    DATA(v_len) = strlen( wl_lin-matnr ).
    IF v_len > 18.
      sl_item-matnr_long = wl_lin-matnr.
    ELSE.
      sl_item-matnr      = wl_lin-matnr.
    ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
    sl_item-maktx   = wl_lin-maktx.
    sl_item-bwkey   = wl_lin-bwkey.
    sl_item-matkl   = wl_lin-matkl.
    sl_item-nbm     = wl_lin-nbm.
    sl_item-charg   = me->zif_controle_retorno_rfl~at_charg.
    sl_item-taxsit  = '50'.
    sl_item-taxsi2  = wl_lin-taxsi2.
    sl_item-matuse  = wl_lin-matuse.
    sl_item-menge   = vl_menge_retorno.
    sl_item-meins   = wl_lin-meins.
    sl_item-itmtyp  = wl_lin-itmtyp.
    sl_item-werks   = wl_lin-werks.
    sl_item-cfop_10 = vl_cfop.
    sl_item-netpr   = vl_netwr_retorno / vl_menge_retorno.
    sl_item-netwr   = vl_netwr_retorno.
    sl_item-matorg  = '0'.
    sl_item-taxlw1  = 'IC5'.
    sl_item-taxlw2  = 'I03'.
    sl_item-taxlw4  = 'C08'.
    sl_item-taxlw5  = 'P08'.

    APPEND sl_item TO tl_item.

*----------------------------------------------------------------------------------*
* Preenche Item ADD
*----------------------------------------------------------------------------------*

    sl_item_add-itmnum = vl_itmnum.
    sl_item_add-direct = 1.
    APPEND sl_item_add TO tl_item_add.

*----------------------------------------------------------------------------------*
* Preenche Item TAX
*----------------------------------------------------------------------------------*

    me->zif_controle_retorno_rfl~preenche_tax( EXPORTING i_taxtyp = 'IPIS' i_itmnum = vl_itmnum i_othbas = vl_netwr_retorno
                                               CHANGING  c_item_tax_tab    = tl_item_tax ).

    me->zif_controle_retorno_rfl~preenche_tax( EXPORTING i_taxtyp = 'IPI3' i_itmnum = vl_itmnum i_othbas = vl_netwr_retorno
                                               CHANGING  c_item_tax_tab    = tl_item_tax ).

    me->zif_controle_retorno_rfl~preenche_tax( EXPORTING i_taxtyp = 'ICM3' i_itmnum = vl_itmnum i_othbas = vl_netwr_retorno
                                               CHANGING  c_item_tax_tab    = tl_item_tax ).

    me->zif_controle_retorno_rfl~preenche_tax( EXPORTING i_taxtyp = 'ICOF' i_itmnum = vl_itmnum i_othbas = vl_netwr_retorno
                                               CHANGING  c_item_tax_tab    = tl_item_tax ).

*----------------------------------------------------------------------------------*
* Preenche OBJ_HEADER_MSG
*----------------------------------------------------------------------------------*

    me->zif_controle_retorno_rfl~preenche_msg( EXPORTING i_linnum  = 1 i_message = 'RETORNO SIMBOLICO DE MERCADORIA REMETIDA P/ FORM. DE LOTE E POSTERIOR.'
                                               CHANGING  c_msg_tab = tl_msg ).

    me->zif_controle_retorno_rfl~preenche_msg( EXPORTING i_linnum  = 2 i_message = 'EXPORTACAO CONF. RELACAO ANEXO.'
                                               CHANGING  c_msg_tab = tl_msg ).

*----------------------------------------------------------------------------------*
*  Cria NF
*----------------------------------------------------------------------------------*

    CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA'  "#EC CI_USAGE_OK[2438131]
      EXPORTING
        obj_header     = sl_header
        obj_header_add = sl_header_add
        nfcheck        = sl_nfcheck
      IMPORTING
        e_docnum       = vl_docnum_retorno
      TABLES
        obj_partner    = tl_partner
        obj_item       = tl_item
        obj_item_add   = tl_item_add
        obj_item_tax   = tl_item_tax
        obj_header_msg = tl_msg
        return         = tl_return.

    IF vl_docnum_retorno IS INITIAL.

      LOOP AT tl_return INTO DATA(wl_return) WHERE type = 'E'.

        RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
          EXPORTING
            textid = VALUE #( msgid = wl_return-id
                              msgno = wl_return-number
                              attr1 = CONV #( wl_return-message_v1 )
                              attr2 = CONV #( wl_return-message_v2 )
                              attr3 = CONV #( wl_return-message_v3 )
                              attr4 = CONV #( wl_return-message_v4 )
                              )
            msgty  = 'E'
            msgno  = wl_return-number
            msgid  = wl_return-id
            msgv1  = CONV #( wl_return-message_v1 )
            msgv2  = CONV #( wl_return-message_v2 )
            msgv3  = CONV #( wl_return-message_v3 )
            msgv4  = CONV #( wl_return-message_v4 ).

      ENDLOOP.

      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_error_gerar_retorno-msgid
                            msgno = zcx_controle_retorno_rfl=>zcx_error_gerar_retorno-msgno
                            )
          msgty  = 'E'
          msgno  = zcx_controle_retorno_rfl=>zcx_error_gerar_retorno-msgno
          msgid  = zcx_controle_retorno_rfl=>zcx_error_gerar_retorno-msgid.

    ELSE.

*----------------------------------------------------------------------------------*
*   Preenchimento NFs Vinculadas ao Retorno
*----------------------------------------------------------------------------------*
      CALL FUNCTION 'ZSDF_GET_DENTRO_FORA_PRAZO'
        IMPORTING
          e_prazo = vl_prazo.

      CLEAR: tl_ret[].
      LOOP AT me->zif_controle_retorno_rfl~at_notas_selecionadas INTO wl_nota_sel WHERE check_sel EQ abap_true.

        CLEAR: sl_ret.

        sl_ret-docnum       = wl_nota_sel-docnum.
        sl_ret-nfenum       = wl_nota_sel-nfenum.
        sl_ret-werks        = wl_nota_sel-werks.
        sl_ret-data_saida   = wl_nota_sel-credat.
        sl_ret-unitario     = wl_nota_sel-netwrt / wl_nota_sel-menge.
        sl_ret-quant_vinc   = wl_nota_sel-qtde_vinc_ret.
        sl_ret-vlr_total    = sl_ret-unitario * sl_ret-quant_vinc.

        sl_ret-nf_retorno   = space.
        sl_ret-data_criacao = sy-datum.
        sl_ret-status       = 'A'.
        sl_ret-docnum_ret   = vl_docnum_retorno.

        sl_ret-bukrs        = me->zif_controle_retorno_rfl~at_bukrs.
        sl_ret-safra        = me->zif_controle_retorno_rfl~at_charg.
        sl_ret-lgort        = me->zif_controle_retorno_rfl~at_lgort.
        sl_ret-matnr        = me->zif_controle_retorno_rfl~at_matnr.
        sl_ret-parceiro     = me->zif_controle_retorno_rfl~at_terminal.

        sl_ret-prazo        = vl_prazo.

        APPEND sl_ret TO tl_ret.

      ENDLOOP.

*----------------------------------------------------------------------------------*
*   Preenchimento Quantidade Exportação
*----------------------------------------------------------------------------------*

      CLEAR: sl_export.

      sl_export-docnum       = vl_docnum_retorno.
      sl_export-werks        = me->zif_controle_retorno_rfl~at_werks.
      sl_export-matnr        = me->zif_controle_retorno_rfl~at_matnr.
      sl_export-quant        = vl_menge_retorno.
      sl_export-valor_total  = vl_netwr_retorno.
      sl_export-data_criacao = sy-datum.
      sl_export-export       = space.
      sl_export-status       = 'A'.
      sl_export-nf_retorno   = space.
      sl_export-finalidade   = i_finalidade.


*----------------------------------------------------------------------------------*
*   Gravar Registros
*----------------------------------------------------------------------------------*

      DATA(_error) = ''.

      IF tl_ret[] IS NOT INITIAL.
        INSERT zsdt_retlote FROM TABLE tl_ret.
        IF sy-subrc NE 0.
          _error = 'X'.
        ENDIF.
      ENDIF.

      IF sl_export IS NOT INITIAL.
        MODIFY zsdt_export FROM sl_export.
        IF sy-subrc NE 0.
          _error = 'X'.
        ENDIF.
      ENDIF.

      IF _error IS INITIAL.

        e_docnum_retorno = vl_docnum_retorno.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
          EXPORTING
            textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_error_gerar_retorno-msgid
                              msgno = zcx_controle_retorno_rfl=>zcx_error_gerar_retorno-msgno
                              )
            msgty  = 'E'
            msgno  = zcx_controle_retorno_rfl=>zcx_error_gerar_retorno-msgno
            msgid  = zcx_controle_retorno_rfl=>zcx_error_gerar_retorno-msgid.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method ZIF_CONTROLE_RETORNO_RFL~GET_INSTANCE.

    IF ZIF_CONTROLE_RETORNO_RFL~AT_IF_CONTROLE_RETORNO_RFL IS NOT BOUND.
      CREATE OBJECT ZIF_CONTROLE_RETORNO_RFL~AT_IF_CONTROLE_RETORNO_RFL TYPE ZCL_CONTROLE_RETORNO_RFL.
    ENDIF.

    R_IF_CONTROLE_RETORNO_RFL = ZIF_CONTROLE_RETORNO_RFL~AT_IF_CONTROLE_RETORNO_RFL.


  endmethod.


  method ZIF_CONTROLE_RETORNO_RFL~NOVO_LANCAMENTO.

   LOOP AT ME->ZIF_CONTROLE_RETORNO_RFL~AT_NOTAS_SELECIONADAS ASSIGNING FIELD-SYMBOL(<FS_NOTA_SEL>) WHERE CHECK_SEL EQ ABAP_TRUE.
     CLEAR: <FS_NOTA_SEL>-QTDE_VINC_RET, <FS_NOTA_SEL>-CHECK_SEL.
   ENDLOOP.

  endmethod.


  method ZIF_CONTROLE_RETORNO_RFL~PREENCHE_MSG.

    DATA SL_MSG TYPE BAPI_J_1BNFFTX.

    SL_MSG-LINNUM  = I_LINNUM.
    SL_MSG-MESSAGE = I_MESSAGE.

    APPEND SL_MSG TO C_MSG_TAB.

  endmethod.


  method ZIF_CONTROLE_RETORNO_RFL~PREENCHE_TAX.

   DATA SL_ITEM_TAX TYPE BAPI_J_1BNFSTX.

   SL_ITEM_TAX-ITMNUM = I_ITMNUM.
   SL_ITEM_TAX-TAXTYP = I_TAXTYP.
   SL_ITEM_TAX-OTHBAS = I_OTHBAS.

  APPEND SL_ITEM_TAX TO C_ITEM_TAX_TAB.

  endmethod.


  METHOD zif_controle_retorno_rfl~selecionar_notas.

    DATA: it_nf TYPE zde_nota_retorno_rfl_t,
          wa_nf TYPE zde_nota_retorno_rfl.

    CLEAR: e_notas[],  me->zif_controle_retorno_rfl~at_notas_selecionadas[], it_nf[].

    me->zif_controle_retorno_rfl~at_bukrs        = i_bukrs.
    me->zif_controle_retorno_rfl~at_fkart        = i_fkart.
    me->zif_controle_retorno_rfl~at_werks        = i_werks.
    me->zif_controle_retorno_rfl~at_lgort        = i_lgort.
    me->zif_controle_retorno_rfl~at_charg        = i_charg.
    me->zif_controle_retorno_rfl~at_kunnr        = i_kunnr.
    me->zif_controle_retorno_rfl~at_terminal     = i_terminal.
    me->zif_controle_retorno_rfl~at_matnr        = i_matnr.
    me->zif_controle_retorno_rfl~at_status_cct   = i_status_cct.


    TRY.
        IF i_dt_ini_emi IS NOT INITIAL.
          EXEC SQL.
            OPEN NOTAS FOR

             SELECT LI.DOCNUM,
                      IT.CHARG,
                      LI.REFTYP,
                      RE.VBELN,
                      DC.CREDAT,
                      DC.PSTDAT,
                      DC.DOCDAT,
                      DC.NFENUM,
                      DC.BUKRS,
                      DC.BRANCH,
                      RE.VSTEL AS CENTRO,
                      IT.LGORT,
                      LI.MATNR,
                      map(LI.MEINS, 'TO', LI.MENGE/1000, LI.MENGE ) AS MENGE,
                      LI.MEINS,
                      map(LI.NETWRT,0,LI.NETWR,LI.NETWRT) / map(LI.MEINS, 'TO', LI.MENGE/1000, LI.MENGE ) AS NETPR,
                      map(LI.NETWRT,0,LI.NETWR,LI.NETWRT) AS NETWRT,
                      map(CC.NOVO_TERMINAL,null,PA.LIFNR, CC.NOVO_TERMINAL) AS TERMINAL,
                      AC.REGIO || AC.NFYEAR || AC.NFMONTH || AC.STCD1 || AC.MODEL || AC.SERIE || AC.NFNUM9 || AC.DOCNUM9 || AC.CDV  AS CHAVE,
                      DC.PARID,
                      DC.PARTYP,
                      ( SELECT coalesce(SUM(QUANT_VINC),0)
                          FROM SAPHANADB.ZSDT_RETLOTE
                         WHERE MANDT  = :SY-MANDT
                           AND DOCNUM = LI.DOCNUM ) AS QTDE_VINC,

                      CCT.PESO_AFERIDO_RECEPCAO AS QTDE_CCT,
                      CCT.DT_RECEPCAO AS DT_CCT,

                      CC.DOC_MATERIAL AS MBLNR_CCE,
                      CC.ANO_MATERIAL AS MJAHR_CCE,

                      CCT.LOCAL_CODIGO_RA AS RA_CCT,
                      DC.CANDAT


                 FROM SAPHANADB.LIKP           RE,
                      SAPHANADB.LIPS           IT,
                      SAPHANADB.VBPA           PA,
                      SAPHANADB.VBRP           FT,
                      SAPHANADB.J_1BNFLIN      LI left join ( SELECT MANDT, DOCNUM, NOVO_TERMINAL, DOC_MATERIAL, ANO_MATERIAL
                                FROM SAPHANADB.ZCARTA_CORRECAO CC
                               WHERE CC.MANDT = :SY-MANDT
                                 AND CC.NOVO_TERMINAL <> ' '
                                 AND CC.AUTHCODE      <> ' '
                                 AND CC.ID_CC = ( SELECT MAX(ID_CC)
                                          FROM SAPHANADB.ZCARTA_CORRECAO CCM
                                         WHERE CCM.MANDT = :SY-MANDT
                                           AND CCM.DOCNUM        = CC.DOCNUM
                                           AND CCM.NOVO_TERMINAL <> ' '
                                           AND CCM.AUTHCODE      <> ' ' ) ) CC

                            on  LI.MANDT     = CC.MANDT
                                AND LI.DOCNUM    = CC.DOCNUM




            ,
                      SAPHANADB.J_1BNFDOC      DC,
                      SAPHANADB.J_1BNFE_ACTIVE AC left join

                      ( SELECT NFCCT.CHAVE_NFE, DT_RECEPCAO, coalesce(CBCCT.PESO_AFERIDO_RECEPCAO,0) AS PESO_AFERIDO_RECEPCAO, CBCCT.LOCAL_CODIGO_RA
                          FROM SAPHANADB.ZLEST0147 NFCCT,
                               SAPHANADB.ZLEST0146 CBCCT
                         WHERE NFCCT.MANDT       = :SY-MANDT
                           AND NFCCT.MANDT       = CBCCT.MANDT
                           AND NFCCT.ID_RECEPCAO = CBCCT.ID_RECEPCAO
                           AND CBCCT.CANCEL      = ' ' ) CCT

                on  AC.REGIO || AC.NFYEAR || AC.NFMONTH || AC.STCD1 || AC.MODEL || AC.SERIE || AC.NFNUM9 || AC.DOCNUM9 || AC.CDV = CCT.CHAVE_NFE



                WHERE RE.MANDT     = :SY-MANDT

                  AND RE.MANDT     = IT.MANDT
                  AND RE.VBELN     = IT.VBELN

                  AND RE.MANDT     = PA.MANDT
                  AND RE.VBELN     = PA.VBELN

                  AND IT.MANDT     = FT.MANDT
                  AND IT.VBELN     = FT.VGBEL
                  AND IT.POSNR     = FT.VGPOS

                  AND FT.MANDT     = LI.MANDT
                  AND FT.VBELN     = LI.REFKEY
                  AND FT.POSNR     = LI.REFITM

                  AND LI.MANDT     = AC.MANDT
                  AND LI.DOCNUM    = AC.DOCNUM



                  AND LI.MANDT     = DC.MANDT
                  AND LI.DOCNUM    = DC.DOCNUM




                  AND AC.CANCEL    = ' '
                  AND AC.DOCSTA    = '1'
                  AND PA.PARVW     = 'Z1'
                  AND LI.MENGE     > 0

                  AND RE.VKORG     = :I_BUKRS
                  AND RE.VSTEL     = :I_WERKS
                  AND RE.FKARV     = :I_FKART
                  AND RE.KUNNR     = :I_KUNNR
                  AND IT.MATNR     = :I_MATNR
                  AND IT.CHARG     = :I_CHARG
                  AND DC.DOCDAT    >= :I_DT_INI_EMI
                  AND DC.DOCDAT    <= :I_DT_FIM_EMI




               UNION ALL

               SELECT L.DOCNUM,
                      I.CHARG,
                      L.REFTYP,
                      ' ' AS VBELN,
                      D.CREDAT,
                      D.PSTDAT,
                      D.DOCDAT,
                      D.NFENUM,
                      D.BUKRS,
                      D.BRANCH,
                      D.BRANCH AS CENTRO,
                      I.LGORT,
                      L.MATNR,
                      map(L.MEINS, 'TO', L.MENGE/1000, L.MENGE ) AS MENGE,
                      L.MEINS,
                      map(L.NETWRT,0,L.NETWR,L.NETWRT) / map(L.MEINS, 'TO', L.MENGE/1000, L.MENGE ) AS NETPR,
                      map(L.NETWRT,0,L.NETWR,L.NETWRT) AS NETWRT,
                      map(C.NOVO_TERMINAL,null,P.PARID, C.NOVO_TERMINAL) AS TERMINAL,
                      A.REGIO || A.NFYEAR || A.NFMONTH || A.STCD1 || A.MODEL || A.SERIE || A.NFNUM9 || A.DOCNUM9 || A.CDV  AS CHAVE,
                      D.PARID,
                      D.PARTYP,

                     ( SELECT coalesce(SUM(QUANT_VINC),0)
                         FROM SAPHANADB.ZSDT_RETLOTE
                        WHERE MANDT  = :SY-MANDT
                          AND DOCNUM = L.DOCNUM ) AS QTDE_VINC,

                     CCT.PESO_AFERIDO_RECEPCAO AS QTDE_CCT,
                     CCT.DT_RECEPCAO AS DT_CCT,

                     C.DOC_MATERIAL AS MBLNR_CCE,
                     C.ANO_MATERIAL AS MJAHR_CCE,

                     CCT.LOCAL_CODIGO_RA AS RA_CCT,
                     D.CANDAT


                FROM SAPHANADB.J_1BNFLIN      L left join  ( SELECT DOCNUM, NOVO_TERMINAL, DOC_MATERIAL, ANO_MATERIAL
                               FROM SAPHANADB.ZCARTA_CORRECAO CC
                              WHERE CC.MANDT = :SY-MANDT
                                AND CC.NOVO_TERMINAL <> ' '
                                AND CC.AUTHCODE      <> ' '
                                AND CC.ID_CC = ( SELECT MAX(ID_CC)
                                         FROM SAPHANADB.ZCARTA_CORRECAO CCM
                                        WHERE CCM.MANDT         = :SY-MANDT
                                          AND CCM.DOCNUM        = CC.DOCNUM
                                          AND CCM.NOVO_TERMINAL <> ' '
                                          AND CCM.AUTHCODE      <> ' ' ) ) C


                          on L.DOCNUM    = C.DOCNUM,

                     SAPHANADB.ZFIWRT0008     Z,
                     SAPHANADB.ZFIWRT0009     I,
                     SAPHANADB.ZFIWRT0015     P,
                     SAPHANADB.J_1BNFDOC      D,
                     SAPHANADB.J_1BNFE_ACTIVE A left join  ( SELECT NFCCT.CHAVE_NFE, DT_RECEPCAO, coalesce(CBCCT.PESO_AFERIDO_RECEPCAO,0) AS PESO_AFERIDO_RECEPCAO, CBCCT.LOCAL_CODIGO_RA
                              FROM SAPHANADB.ZLEST0147 NFCCT,
                                 SAPHANADB.ZLEST0146 CBCCT
                               WHERE NFCCT.MANDT       = :SY-MANDT
                               AND NFCCT.MANDT       = CBCCT.MANDT
                               AND NFCCT.ID_RECEPCAO = CBCCT.ID_RECEPCAO
                               AND CBCCT.CANCEL      = ' ' ) CCT

                                on A.REGIO || A.NFYEAR || A.NFMONTH || A.STCD1 || A.MODEL || A.SERIE || A.NFNUM9 || A.DOCNUM9 || A.CDV = CCT.CHAVE_NFE




               WHERE L.MANDT     = :SY-MANDT
                 AND L.REFTYP    = 'ZW'
                 AND I.CHARG     = :I_CHARG
                 AND L.MANDT     = Z.MANDT
                 AND L.DOCNUM    = Z.DOCNUM
                 AND L.MANDT     = D.MANDT
                 AND L.DOCNUM    = D.DOCNUM
                 AND L.MANDT     = A.MANDT
                 AND L.DOCNUM    = A.DOCNUM



                 AND Z.MANDT     = P.MANDT
                 AND Z.SEQ_LCTO  = P.SEQ_LCTO
                 AND Z.MANDT     = I.MANDT
                 AND Z.SEQ_LCTO  = I.SEQ_LCTO
                 AND P.PARVW     = 'Z1'
                 AND Z.CTRL_ZRFL = 'S'
                 AND A.CANCEL    = ' '
                 AND A.DOCSTA    = '1'
                 AND L.MENGE     > 0
                 AND D.BRANCH    = :I_WERKS
                 AND L.MATNR     = :I_MATNR
                 AND D.DOCDAT    >= :I_DT_INI_EMI
                 AND D.DOCDAT    <= :I_DT_FIM_EMI



          ENDEXEC.

        ELSE.

          "Anderson Oenning
          EXEC SQL.
            OPEN NOTAS FOR

               SELECT LI.DOCNUM,
                      IT.CHARG,
                      LI.REFTYP,
                      RE.VBELN,
                      DC.CREDAT,
                      DC.PSTDAT,
                      DC.DOCDAT,
                      DC.NFENUM,
                      DC.BUKRS,
                      DC.BRANCH,
                      RE.VSTEL AS CENTRO,
                      IT.LGORT,
                      LI.MATNR,
                      map(LI.MEINS, 'TO', LI.MENGE/1000, LI.MENGE ) AS MENGE,
                      LI.MEINS,
                      map(LI.NETWRT,0,LI.NETWR,LI.NETWRT) / map(LI.MEINS, 'TO', LI.MENGE/1000, LI.MENGE ) AS NETPR,
                      map(LI.NETWRT,0,LI.NETWR,LI.NETWRT) AS NETWRT,
                      map(CC.NOVO_TERMINAL,null,PA.LIFNR, CC.NOVO_TERMINAL) AS TERMINAL,
                      AC.REGIO || AC.NFYEAR || AC.NFMONTH || AC.STCD1 || AC.MODEL || AC.SERIE || AC.NFNUM9 || AC.DOCNUM9 || AC.CDV  AS CHAVE,
                      DC.PARID,
                      DC.PARTYP,
                      ( SELECT coalesce(SUM(QUANT_VINC),0)
                          FROM SAPHANADB.ZSDT_RETLOTE
                         WHERE MANDT  = :SY-MANDT
                           AND DOCNUM = LI.DOCNUM ) AS QTDE_VINC,

                      CCT.PESO_AFERIDO_RECEPCAO AS QTDE_CCT,
                      CCT.DT_RECEPCAO AS DT_CCT,

                      CC.DOC_MATERIAL AS MBLNR_CCE,
                      CC.ANO_MATERIAL AS MJAHR_CCE,

                      CCT.LOCAL_CODIGO_RA AS RA_CCT,
                      DC.CANDAT


                 FROM SAPHANADB.LIKP           RE,
                      SAPHANADB.LIPS           IT,
                      SAPHANADB.VBPA           PA,
                      SAPHANADB.VBRP           FT,
                      SAPHANADB.J_1BNFLIN      LI left join      ( SELECT MANDT, DOCNUM, NOVO_TERMINAL, DOC_MATERIAL, ANO_MATERIAL
                                    FROM SAPHANADB.ZCARTA_CORRECAO CC
                                   WHERE CC.MANDT = :SY-MANDT
                                     AND CC.NOVO_TERMINAL <> ' '
                                     AND CC.AUTHCODE      <> ' '
                                     AND CC.ID_CC = ( SELECT MAX(ID_CC)
                                              FROM SAPHANADB.ZCARTA_CORRECAO CCM
                                             WHERE CCM.MANDT = :SY-MANDT
                                               AND CCM.DOCNUM        = CC.DOCNUM
                                               AND CCM.NOVO_TERMINAL <> ' '
                                               AND CCM.AUTHCODE      <> ' ' ) ) CC
                                        on LI.MANDT     = CC.MANDT
                         AND LI.DOCNUM    = CC.DOCNUM,



                      SAPHANADB.J_1BNFDOC      DC,
                      SAPHANADB.J_1BNFE_ACTIVE AC left join ( SELECT NFCCT.CHAVE_NFE, DT_RECEPCAO, coalesce(CBCCT.PESO_AFERIDO_RECEPCAO,0) AS PESO_AFERIDO_RECEPCAO, CBCCT.LOCAL_CODIGO_RA
                                FROM SAPHANADB.ZLEST0147 NFCCT,
                                   SAPHANADB.ZLEST0146 CBCCT
                               WHERE NFCCT.MANDT       = :SY-MANDT
                                 AND NFCCT.MANDT       = CBCCT.MANDT
                                 AND NFCCT.ID_RECEPCAO = CBCCT.ID_RECEPCAO
                                 AND CBCCT.CANCEL      = ' ' ) CCT

                          on AC.REGIO || AC.NFYEAR || AC.NFMONTH || AC.STCD1 || AC.MODEL || AC.SERIE || AC.NFNUM9 || AC.DOCNUM9 || AC.CDV = CCT.CHAVE_NFE







                WHERE RE.MANDT     = :SY-MANDT

                  AND RE.MANDT     = IT.MANDT
                  AND RE.VBELN     = IT.VBELN

                  AND RE.MANDT     = PA.MANDT
                  AND RE.VBELN     = PA.VBELN

                  AND IT.MANDT     = FT.MANDT
                  AND IT.VBELN     = FT.VGBEL
                  AND IT.POSNR     = FT.VGPOS

                  AND FT.MANDT     = LI.MANDT
                  AND FT.VBELN     = LI.REFKEY
                  AND FT.POSNR     = LI.REFITM

                  AND LI.MANDT     = AC.MANDT
                  AND LI.DOCNUM    = AC.DOCNUM



                  AND LI.MANDT     = DC.MANDT
                  AND LI.DOCNUM    = DC.DOCNUM




                  AND AC.CANCEL    = ' '
                  AND AC.DOCSTA    = '1'
                  AND PA.PARVW     = 'Z1'
                  AND LI.MENGE     > 0

                  AND RE.VKORG     = :I_BUKRS
                  AND RE.VSTEL     = :I_WERKS
                  AND RE.FKARV     = :I_FKART
                  AND RE.KUNNR     = :I_KUNNR
                  AND IT.MATNR     = :I_MATNR
                  AND IT.CHARG     = :I_CHARG

               UNION ALL

               SELECT L.DOCNUM,
                      I.CHARG,
                      L.REFTYP,
                      ' ' AS VBELN,
                      D.CREDAT,
                      D.PSTDAT,
                      D.DOCDAT,
                      D.NFENUM,
                      D.BUKRS,
                      D.BRANCH,
                      D.BRANCH AS CENTRO,
                      I.LGORT,
                      L.MATNR,
                      map(L.MEINS, 'TO', L.MENGE/1000, L.MENGE ) AS MENGE,
                      L.MEINS,
                      map(L.NETWRT,0,L.NETWR,L.NETWRT) / map(L.MEINS, 'TO', L.MENGE/1000, L.MENGE ) AS NETPR,
                      map(L.NETWRT,0,L.NETWR,L.NETWRT) AS NETWRT,
                      map(C.NOVO_TERMINAL,null,P.PARID, C.NOVO_TERMINAL) AS TERMINAL,
                      A.REGIO || A.NFYEAR || A.NFMONTH || A.STCD1 || A.MODEL || A.SERIE || A.NFNUM9 || A.DOCNUM9 || A.CDV  AS CHAVE,
                      D.PARID,
                      D.PARTYP,

                     ( SELECT coalesce(SUM(QUANT_VINC),0)
                         FROM SAPHANADB.ZSDT_RETLOTE
                        WHERE MANDT  = :SY-MANDT
                          AND DOCNUM = L.DOCNUM ) AS QTDE_VINC,

                     CCT.PESO_AFERIDO_RECEPCAO AS QTDE_CCT,
                     CCT.DT_RECEPCAO AS DT_CCT,

                     C.DOC_MATERIAL AS MBLNR_CCE,
                     C.ANO_MATERIAL AS MJAHR_CCE,

                     CCT.LOCAL_CODIGO_RA AS RA_CCT,
                     D.CANDAT


                FROM SAPHANADB.J_1BNFLIN      L left join    ( SELECT DOCNUM, NOVO_TERMINAL, DOC_MATERIAL, ANO_MATERIAL
                                 FROM SAPHANADB.ZCARTA_CORRECAO CC
                                WHERE CC.MANDT = :SY-MANDT
                                  AND CC.NOVO_TERMINAL <> ' '
                                  AND CC.AUTHCODE      <> ' '
                                  AND CC.ID_CC = ( SELECT MAX(ID_CC)
                                           FROM SAPHANADB.ZCARTA_CORRECAO CCM
                                          WHERE CCM.MANDT         = :SY-MANDT
                                            AND CCM.DOCNUM        = CC.DOCNUM
                                            AND CCM.NOVO_TERMINAL <> ' '
                                            AND CCM.AUTHCODE      <> ' ' ) ) C

                                                           on L.DOCNUM    = C.DOCNUM,


                     SAPHANADB.ZFIWRT0008     Z,
                     SAPHANADB.ZFIWRT0009     I,
                     SAPHANADB.ZFIWRT0015     P,
                     SAPHANADB.J_1BNFDOC      D,
                     SAPHANADB.J_1BNFE_ACTIVE A left join    ( SELECT NFCCT.CHAVE_NFE, DT_RECEPCAO, coalesce(CBCCT.PESO_AFERIDO_RECEPCAO,0) AS PESO_AFERIDO_RECEPCAO, CBCCT.LOCAL_CODIGO_RA
                        FROM SAPHANADB.ZLEST0147 NFCCT,
                             SAPHANADB.ZLEST0146 CBCCT
                       WHERE NFCCT.MANDT       = :SY-MANDT
                         AND NFCCT.MANDT       = CBCCT.MANDT
                         AND NFCCT.ID_RECEPCAO = CBCCT.ID_RECEPCAO
                         AND CBCCT.CANCEL      = ' ' ) CCT

             on A.REGIO || A.NFYEAR || A.NFMONTH || A.STCD1 || A.MODEL || A.SERIE || A.NFNUM9 || A.DOCNUM9 || A.CDV = CCT.CHAVE_NFE






               WHERE L.MANDT     = :SY-MANDT
                 AND L.REFTYP    = 'ZW'
                 AND I.CHARG     = :I_CHARG
                 AND L.MANDT     = Z.MANDT
                 AND L.DOCNUM    = Z.DOCNUM
                 AND L.MANDT     = D.MANDT
                 AND L.DOCNUM    = D.DOCNUM
                 AND L.MANDT     = A.MANDT
                 AND L.DOCNUM    = A.DOCNUM



                 AND Z.MANDT     = P.MANDT
                 AND Z.SEQ_LCTO  = P.SEQ_LCTO
                 AND Z.MANDT     = I.MANDT
                 AND Z.SEQ_LCTO  = I.SEQ_LCTO

                 AND P.PARVW     = 'Z1'
                 AND Z.CTRL_ZRFL = 'S'
                 AND A.CANCEL    = ' '
                 AND A.DOCSTA    = '1'
                 AND L.MENGE     > 0
                 AND D.BRANCH    = :I_WERKS
                 AND L.MATNR     = :I_MATNR
          ENDEXEC.

        ENDIF.

      CATCH cx_sy_native_sql_error INTO DATA(exc_ref).
        DATA(error_text) = exc_ref->get_text( ).
        MESSAGE error_text TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.


    DO.
      EXEC SQL.
        FETCH NEXT NOTAS INTO
          :WA_NF-DOCNUM,
          :WA_NF-CHARG,
          :WA_NF-REFTYP,
          :WA_NF-VBELN_VL,
          :WA_NF-CREDAT,
          :WA_NF-PSTDAT,
          :WA_NF-DOCDAT,
          :WA_NF-NFENUM,
          :WA_NF-BUKRS,
          :WA_NF-BRANCH,
          :WA_NF-WERKS,
          :WA_NF-LGORT,
          :WA_NF-MATNR,
          :WA_NF-MENGE,
          :WA_NF-MEINS,
          :WA_NF-NETPR,
          :WA_NF-NETWRT,
          :WA_NF-LIFNR_Z1,
          :WA_NF-CHAVE_NFE,
          :WA_NF-PARID,
          :WA_NF-PARTYP,
          :WA_NF-QTDE_VINC,
          :WA_NF-QTDE_CCT,
          :WA_NF-DT_RECEPCAO_CCT,
          :WA_NF-MBLNR_CCE,
          :WA_NF-MJAHR_CCE,
          :WA_NF-RA_CCT,
          :WA_NF-CANDAT

      ENDEXEC.

      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        APPEND wa_nf TO it_nf.
        CLEAR: wa_nf.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE NOTAS
    ENDEXEC.

    DELETE it_nf WHERE parid NE i_kunnr.
    DELETE it_nf WHERE candat NE '00000000'.

    IF i_terminal IS NOT INITIAL.
      DELETE it_nf WHERE lifnr_z1 NE i_terminal.
    ENDIF.

**Inicio CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson
*    IF i_lgort IS NOT INITIAL.
*      DELETE it_nf WHERE lgort NE i_lgort.
*    ENDIF.
**Fim CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson

    IF i_dt_ini_rec IS NOT INITIAL AND i_dt_fim_rec IS NOT INITIAL.
      DELETE it_nf WHERE dt_recepcao_cct NOT BETWEEN  i_dt_ini_rec AND i_dt_fim_rec.
    ENDIF.

    CASE i_status_cct.
      WHEN zif_controle_retorno_rfl=>st_com_cct.
        DELETE it_nf WHERE qtde_cct = 0.
      WHEN zif_controle_retorno_rfl=>st_sem_cct.
        DELETE it_nf WHERE qtde_cct > 0.
    ENDCASE.

    LOOP AT it_nf ASSIGNING FIELD-SYMBOL(<fs_nota>).

      <fs_nota>-saldo_nf =  <fs_nota>-menge - <fs_nota>-qtde_vinc.

      "tEMP DEV 200..
      "<FS_NOTA>-QTDE_CCT = <FS_NOTA>-MENGE - 30.
      "TEMP DEV 200

      IF <fs_nota>-qtde_cct > 0.

        "Saldo CCT
        <fs_nota>-saldo_cct    = <fs_nota>-qtde_cct - <fs_nota>-qtde_vinc.

        IF <fs_nota>-saldo_cct < 0.
          <fs_nota>-saldo_cct = 0.
        ENDIF.

        "Quebra/Sobra
        IF ( <fs_nota>-menge > <fs_nota>-qtde_cct ).
          <fs_nota>-qtde_quebra  = <fs_nota>-menge - <fs_nota>-qtde_cct.
        ELSEIF ( <fs_nota>-qtde_cct > <fs_nota>-menge ). "Sobra
          <fs_nota>-qtde_sobra   = <fs_nota>-qtde_cct - <fs_nota>-menge.
        ENDIF.

      ENDIF.

    ENDLOOP.

    IF i_com_saldo EQ abap_true.
      DELETE it_nf WHERE saldo_nf <= 0.
    ENDIF.

    e_notas[] = it_nf[].
    me->zif_controle_retorno_rfl~at_notas_selecionadas[] = it_nf[].

  ENDMETHOD.


  METHOD zif_controle_retorno_rfl~set_qtde_vinc_nf.

    READ TABLE me->zif_controle_retorno_rfl~at_notas_selecionadas ASSIGNING FIELD-SYMBOL(<fs_nota_sel>) WITH KEY docnum = i_nota_vinc-docnum.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_nf_vinc_not_selected-msgid
                            msgno = zcx_controle_retorno_rfl=>zcx_nf_vinc_not_selected-msgno
                            attr1 = CONV #( i_nota_vinc-docnum )
                            )
          msgty  = 'E'
          msgno  = zcx_controle_retorno_rfl=>zcx_nf_vinc_not_selected-msgno
          msgid  = zcx_controle_retorno_rfl=>zcx_nf_vinc_not_selected-msgid
          msgv1  = CONV #( i_nota_vinc-docnum ).
    ENDIF.

    IF i_nota_vinc-qtde_vinc > <fs_nota_sel>-saldo_nf.
      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_erro_geral-msgid
                            msgno = zcx_controle_retorno_rfl=>zcx_erro_geral-msgno
                            attr1 = CONV #( 'Documento Fiscal sem saldo p/ retorno! Documento:' )
                            attr2 = CONV #( i_nota_vinc-docnum )
                            )
          msgty  = 'E'
          msgno  = zcx_controle_retorno_rfl=>zcx_erro_geral-msgno
          msgid  = zcx_controle_retorno_rfl=>zcx_erro_geral-msgid
          msgv1  = CONV #( 'Documento Fiscal sem saldo p/ retorno! Documento:' )
          msgv2  = CONV #( i_nota_vinc-docnum ).
    ENDIF.

    <fs_nota_sel>-qtde_vinc_ret = i_nota_vinc-qtde_vinc.
    <fs_nota_sel>-check_sel     = abap_true.

  ENDMETHOD.


  METHOD zif_controle_retorno_rfl~validar_quantidade_retorno.

    DATA: tg_lin        TYPE TABLE OF j_1bnflin.
    DATA: vl_qtde_nota    TYPE j_1bnflin-menge,
          vl_qtde_dif     TYPE j_1bnflin-menge,
          vl_qtde_vinc_nf TYPE j_1bnflin-menge,
          vl_menge_aux    TYPE j_1bnflin-menge.

    SELECT SUM( quant_vinc )
      FROM zsdt_retlote INTO vl_qtde_vinc_nf
     WHERE docnum = i_docnum.

    SELECT *
      FROM j_1bnflin INTO TABLE tg_lin
     WHERE docnum = i_docnum.

    IF tg_lin[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_item_nf_not_found-msgid
                            msgno = zcx_controle_retorno_rfl=>zcx_item_nf_not_found-msgno
                            attr1 = CONV #( i_docnum )
                            )
          msgty  = 'E'
          msgno  = zcx_controle_retorno_rfl=>zcx_item_nf_not_found-msgno
          msgid  = zcx_controle_retorno_rfl=>zcx_item_nf_not_found-msgid
          msgv1  = CONV #( i_docnum ).
    ENDIF.

    LOOP AT tg_lin INTO DATA(wl_lin).

      IF wl_lin-meins NE 'KG'.
        CLEAR: vl_menge_aux.

        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = wl_lin-matnr
            i_mein1             = wl_lin-meins
            i_meins             = 'KG'
            i_menge             = wl_lin-menge
          IMPORTING
            menge               = vl_menge_aux
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.

        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
            EXPORTING
              textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_erro_geral-msgid
                                msgno = zcx_controle_retorno_rfl=>zcx_erro_geral-msgno
                                attr1 = CONV #( 'Erro conversão quantidade documento' )
                                attr2 = CONV #( i_docnum )
                                )
              msgty  = 'E'
              msgno  = zcx_controle_retorno_rfl=>zcx_erro_geral-msgno
              msgid  = zcx_controle_retorno_rfl=>zcx_erro_geral-msgid
              msgv1  = CONV #( 'Erro conversão quantidade documento' )
              msgv2  = CONV #( i_docnum ).
        ENDIF.

        ADD vl_menge_aux TO vl_qtde_nota.
      ELSE.
        ADD wl_lin-menge TO vl_qtde_nota.
      ENDIF.

    ENDLOOP.

    IF ( vl_qtde_vinc_nf + i_menge_vinc ) > vl_qtde_nota.

      vl_qtde_dif = ( vl_qtde_vinc_nf + i_menge_vinc ) - vl_qtde_nota.

      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_erro_geral-msgid
                            msgno = zcx_controle_retorno_rfl=>zcx_erro_geral-msgno
                            attr1 = CONV #( 'Quantidade do documento excedida ' )
                            attr2 = CONV #( i_docnum )
                            attr3  = CONV #( vl_qtde_dif )
                            )
          msgty  = 'E'
          msgno  = zcx_controle_retorno_rfl=>zcx_erro_geral-msgno
          msgid  = zcx_controle_retorno_rfl=>zcx_erro_geral-msgid
          msgv1  = CONV #( 'Quantidade do documento excedida' )
          msgv2  = CONV #( i_docnum )
          msgv3  = CONV #( vl_qtde_dif ).

    ENDIF.

  ENDMETHOD.


  method ZIF_CONTROLE_RETORNO_RFL~VALIDA_RETORNO.

  DATA: VL_QTDE_NOTA        TYPE J_1BNFLIN-MENGE,
        VL_QTDE_VINC_NF     TYPE J_1BNFLIN-MENGE,
        VL_QTDE_DIF         TYPE J_1BNFLIN-MENGE,
        VL_MENGE_AUX        TYPE J_1BNFLIN-MENGE,
        VL_NETWR            TYPE J_1BNFLIN-NETWRT.

  DATA: VL_TERMINAL_RETORNO TYPE LFA1-LIFNR.

  DATA: TG_LIN        TYPE TABLE OF J_1BNFLIN.

  DATA: WL_RET_LOTE_QUEBRA TYPE ZSDT_RETLOTE.

  CLEAR: E_NETWR_RETORNO, E_MENGE_RETORNO, VL_TERMINAL_RETORNO.

  LOOP AT ME->ZIF_CONTROLE_RETORNO_RFL~AT_NOTAS_SELECIONADAS INTO DATA(WL_NOTA_SEL) WHERE CHECK_SEL EQ ABAP_TRUE.

    CLEAR: VL_QTDE_NOTA, VL_QTDE_VINC_NF, VL_QTDE_DIF, VL_NETWR, TG_LIN[].

    IF VL_TERMINAL_RETORNO IS INITIAL.
      VL_TERMINAL_RETORNO = WL_NOTA_SEL-LIFNR_Z1.
    ENDIF.

    IF WL_NOTA_SEL-LIFNR_Z1 NE VL_TERMINAL_RETORNO.
      RAISE EXCEPTION TYPE ZCX_CONTROLE_RETORNO_RFL
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CONTROLE_RETORNO_RFL=>ZCX_TERMINAIS_NF_RET_DIFF-MSGID
                              MSGNO = ZCX_CONTROLE_RETORNO_RFL=>ZCX_TERMINAIS_NF_RET_DIFF-MSGNO
                              )
            MSGTY  = 'E'
            MSGNO  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_TERMINAIS_NF_RET_DIFF-MSGNO
            MSGID  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_TERMINAIS_NF_RET_DIFF-MSGID.
    ENDIF.


    IF I_CHECK_EXISTS_RET_FINALIDADE EQ ABAP_TRUE.
*{   REPLACE        DEVK9A2U9E                                        1
*\      SELECT SINGLE *
*\        FROM ZSDT_RETLOTE AS A INTO WL_RET_LOTE_QUEBRA
*\       WHERE A~DOCNUM = WL_NOTA_SEL-DOCNUM
*\         AND EXISTS ( SELECT * FROM ZSDT_EXPORT AS E WHERE E~DOCNUM = A~DOCNUM_RET AND E~FINALIDADE = i_finalidade ).
*\
*\      IF SY-SUBRC EQ 0.
*\        RAISE EXCEPTION TYPE ZCX_CONTROLE_RETORNO_RFL
*\           EXPORTING
*\             TEXTID = VALUE #( MSGID = ZCX_CONTROLE_RETORNO_RFL=>ZCX_EXISTS_RETORNO_QUEBRA-MSGID
*\                               MSGNO = ZCX_CONTROLE_RETORNO_RFL=>ZCX_EXISTS_RETORNO_QUEBRA-MSGNO
*\                               ATTR1 = CONV #( WL_RET_LOTE_QUEBRA-DOCNUM_RET )
*\                               ATTR2 = CONV #( WL_NOTA_SEL-DOCNUM )
*\                               )
*\             MSGTY  = 'E'
*\             MSGNO  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_EXISTS_RETORNO_QUEBRA-MSGNO
*\             MSGID  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_EXISTS_RETORNO_QUEBRA-MSGID
*\             MSGV1   = CONV #( WL_RET_LOTE_QUEBRA-DOCNUM_RET )
*\             MSGV2   = CONV #( WL_NOTA_SEL-DOCNUM ).
*\      ENDIF.
* Logica acima comentada para que possa ser feito mais de um retorno por Quebra Porto - Stefanini - 09/10/2025 - IR261706
*}   REPLACE
    ENDIF.

    SELECT SUM( QUANT_VINC )
      FROM ZSDT_RETLOTE INTO VL_QTDE_VINC_NF
     WHERE DOCNUM = WL_NOTA_SEL-DOCNUM.

    SELECT *
      FROM J_1BNFLIN INTO TABLE TG_LIN
     WHERE DOCNUM = WL_NOTA_SEL-DOCNUM.

    IF TG_LIN[] IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CONTROLE_RETORNO_RFL
         EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_CONTROLE_RETORNO_RFL=>ZCX_ITEM_NF_NOT_FOUND-MSGID
                             MSGNO = ZCX_CONTROLE_RETORNO_RFL=>ZCX_ITEM_NF_NOT_FOUND-MSGNO
                             ATTR1 = CONV #( WL_NOTA_SEL-DOCNUM )
                             )
           MSGTY  = 'E'
           MSGNO  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_ITEM_NF_NOT_FOUND-MSGNO
           MSGID  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_ITEM_NF_NOT_FOUND-MSGID
           MSGV1   = CONV #( WL_NOTA_SEL-DOCNUM ).
    ENDIF.

    LOOP AT TG_LIN INTO DATA(WL_LIN).

      IF WL_LIN-MEINS NE 'KG'.
        CLEAR: VL_MENGE_AUX.

        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            I_MATNR             = WL_LIN-MATNR
            I_MEIN1             = WL_LIN-MEINS
            I_MEINS             = 'KG'
            I_MENGE             = WL_LIN-MENGE
          IMPORTING
            MENGE               = VL_MENGE_AUX
          EXCEPTIONS
            ERROR_IN_CONVERSION = 1
            NO_SUCCESS          = 2
            OTHERS              = 3.

        IF SY-SUBRC NE 0.
          RAISE EXCEPTION TYPE ZCX_CONTROLE_RETORNO_RFL
             EXPORTING
               TEXTID = VALUE #( MSGID = ZCX_CONTROLE_RETORNO_RFL=>ZCX_ERRO_GERAL-MSGID
                                 MSGNO = ZCX_CONTROLE_RETORNO_RFL=>ZCX_ERRO_GERAL-MSGNO
                                 ATTR1 = CONV #( 'Erro conversão quantidade documento' )
                                 ATTR2 = CONV #( WL_NOTA_SEL-DOCNUM )
                                 )
               MSGTY  = 'E'
               MSGNO  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_ERRO_GERAL-MSGNO
               MSGID  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_ERRO_GERAL-MSGID
               MSGV1   = CONV #( 'Erro conversão quantidade documento' )
               MSGV2   = CONV #( WL_NOTA_SEL-DOCNUM ).
        ENDIF.

        ADD VL_MENGE_AUX TO VL_QTDE_NOTA.
      ELSE.
        ADD WL_LIN-MENGE TO VL_QTDE_NOTA.
      ENDIF.

    ENDLOOP.

    IF ( VL_QTDE_VINC_NF + WL_NOTA_SEL-QTDE_VINC_RET ) > VL_QTDE_NOTA.

      VL_QTDE_DIF = ( VL_QTDE_VINC_NF + WL_NOTA_SEL-QTDE_VINC_RET ) - VL_QTDE_NOTA.

      RAISE EXCEPTION TYPE ZCX_CONTROLE_RETORNO_RFL
         EXPORTING
           TEXTID = VALUE #( MSGID = ZCX_CONTROLE_RETORNO_RFL=>ZCX_QTDE_ITEM_EXCEDIDA-MSGID
                             MSGNO = ZCX_CONTROLE_RETORNO_RFL=>ZCX_QTDE_ITEM_EXCEDIDA-MSGNO
                             ATTR1 = CONV #( WL_NOTA_SEL-DOCNUM )
                             ATTR2 = CONV #( VL_QTDE_DIF )
                             )
           MSGTY  = 'E'
           MSGNO  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_QTDE_ITEM_EXCEDIDA-MSGNO
           MSGID  = ZCX_CONTROLE_RETORNO_RFL=>ZCX_QTDE_ITEM_EXCEDIDA-MSGID
           MSGV1   = CONV #( WL_NOTA_SEL-DOCNUM )
           MSGV2   = CONV #( VL_QTDE_DIF ).

    ENDIF.

    VL_NETWR = WL_NOTA_SEL-QTDE_VINC_RET * WL_NOTA_SEL-NETPR.

    ADD VL_NETWR                  TO E_NETWR_RETORNO.
    ADD WL_NOTA_SEL-QTDE_VINC_RET TO E_MENGE_RETORNO.

  ENDLOOP.

  endmethod.


  METHOD zif_controle_retorno_rfl~atualizar_tabelas_z.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZIF_CONTROLE_RETORNO_RFL~ATUALIZAR_TABELAS_Z                                         *
*& Chamado        : USER STORY 157683                                                                    *
*& Data           : 14/11/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 14/11/2024|DEVK9A1XAW |NSEGATIN       | Atualização das tabelas de reversão (estorno) da NF Retorno   *
*--------------------------------------------------------------------------------------------------------*

* Reverter (estornar) o retorno de formação de lote.
    zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->reverte_retorno_1x1( EXPORTING i_docnum = i_docnum ).

    DELETE FROM zsdt_export  WHERE docnum EQ i_docnum.

    DELETE FROM zsdt_retlote WHERE docnum_ret EQ i_docnum.

    SELECT COUNT( DISTINCT docnum_rt )
      FROM znom_remetente INTO @DATA(lv_cont_memo)
     WHERE docnum_rt EQ @i_docnum.

    IF lv_cont_memo IS NOT INITIAL.
      UPDATE znom_remetente SET docnum_rt = space WHERE docnum_rt EQ i_docnum.

    ENDIF.

    SELECT SINGLE * FROM zsdt0053 INTO @DATA(_wl_0053) WHERE docnum_rt EQ @i_docnum.

    IF sy-subrc IS INITIAL.
      UPDATE zsdt0053 SET docnum_rt = '0000000000' WHERE docnum_rt EQ i_docnum.

    ENDIF.

  ENDMETHOD.


  METHOD zif_controle_retorno_rfl~cnst_e_valida_term_nfe_saida.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZIF_CONTROLE_RETORNO_RFL~CNST_E_VALIDA_TERM_NFE_SAIDA                                  *
*& Chamado        : USER STORY 163355                                                                    *
*& Data           : 04/02/2025                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*& 04/02/2025|DEVK9A1XAW |NSEGATIN       | Consulta/Valida Term. de Embarque de NFe Saída por RA e EUDR  *
*--------------------------------------------------------------------------------------------------------*
*& 14/02/2025|DEVK9A1XAW |NSEGATIN       | Ajuste da pesquisa da NFs de Saída RFL quando marcado para    *
*&                                       |exibir somente notas EUDR = SIM [165835].                      *
*--------------------------------------------------------------------------------------------------------*

    DATA: tl_docnum            TYPE j_1bnfe_t_docnum,
          tl_cnslt_terminal_ax TYPE zsdct_consulta_terminal.

    DATA: rl_docnum TYPE          rsis_t_range,
          rl_eudr   TYPE RANGE OF zeudr.

    DATA: el_zlest0146_s TYPE zlest0146.

    DATA: vl_doc_rateio TYPE c,
          vl_saida_cct  TYPE c.

    LOOP AT it_docnum INTO DATA(el_doc).
      CLEAR tl_docnum.
* Verifica o tipo de movimento da NF.
      CASE i_direct.
        WHEN sy-abcde+4(1).  "E - Entrada
* Verificar se a NF Entrada já está vinculada.
          SELECT docnum_flote FROM zsdtvinc_p_flote
            INTO TABLE tl_docnum
          WHERE docnum_eprod EQ el_doc-docnum
            AND cancel       EQ abap_false.

          CHECK sy-subrc IS INITIAL.

        WHEN sy-abcde+18(1). "S - Saída
          APPEND el_doc-docnum TO tl_docnum.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.
* Consulta Terminal de Embarque NFe - Parceito Negócio Tp. Z1
      zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->consulta_terminar_nfe(
      EXPORTING
        it_docnum = tl_docnum
      IMPORTING
        et_consulta_terminal = tl_cnslt_terminal_ax[] ).

      CHECK tl_cnslt_terminal_ax[] IS NOT INITIAL.
* Busca o terminal de embarque.
      SELECT SINGLE lifnr FROM zsdt0168 INTO @DATA(vl_terminal) WHERE codigo_ra EQ @i_cod_ra_embarque.

      CHECK sy-subrc IS INITIAL AND vl_terminal IS NOT INITIAL.

      LOOP AT tl_cnslt_terminal_ax INTO DATA(el_cnslt_terminal_ax).
        DATA(vl_tabix) = sy-tabix.
        IF el_cnslt_terminal_ax-lifnr NE vl_terminal.
          DELETE tl_cnslt_terminal_ax INDEX vl_tabix.

        ELSE.
          CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
            EXPORTING
              i_docnum            = el_cnslt_terminal_ax-docnum
              i_set_peso_disp_uso = abap_false
            IMPORTING
              e_zlest0146         = el_zlest0146_s
              e_doc_rateio        = vl_doc_rateio.

          IF  vl_doc_rateio  IS INITIAL AND NOT el_zlest0146_s IS INITIAL.
            IF el_zlest0146_s-local_codigo_ra NE i_cod_ra_embarque.
              DELETE tl_cnslt_terminal_ax INDEX vl_tabix.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

      CHECK NOT tl_cnslt_terminal_ax[] IS INITIAL.

*** Verifica se é Nota EUDR, qual o tipo e valida as condições.
      FREE: rl_eudr.

      CASE i_eudr.
        WHEN zcl_eudr_utils=>lc_s_eudr.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = zcl_eudr_utils=>lc_s_eudr ) TO rl_eudr.

        WHEN zcl_eudr_utils=>lc_n_eudr OR space.
          IF i_retornar_eudr IS INITIAL. "Lista tambem notas EUDR?
            APPEND VALUE #( sign = 'I' option = 'NE' low = zcl_eudr_utils=>lc_s_eudr ) TO rl_eudr.
**<<<------"165835 - NMS - INI------>>>
          ELSE.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = zcl_eudr_utils=>lc_s_eudr ) TO rl_eudr.
**<<<------"165835 - NMS - FIM------>>>
          ENDIF.

      ENDCASE.

      rl_docnum = VALUE #( FOR el_lin IN tl_cnslt_terminal_ax ( sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
                                                                option = zcl_les_utils=>if_stab_constants~mc_option_equal
                                                                low    = el_lin-docnum
                                                              )
                         ).

      zcl_eudr_utils=>check_doc_fiscal_eudr( EXPORTING
                                               i_docnum_t    =  rl_docnum
                                             IMPORTING
                                               e_docnum_eudr =  DATA(tl_docnum_eudr)
                                            ).

      DELETE tl_docnum_eudr WHERE eudr NOT IN rl_eudr.

      IF tl_docnum_eudr[] IS INITIAL.
        CLEAR tl_cnslt_terminal_ax[].
        CONTINUE.

      ENDIF.

      FREE rl_docnum.

      rl_docnum = VALUE #( FOR el_docnum IN tl_docnum_eudr ( sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
                                                             option = zcl_les_utils=>if_stab_constants~mc_option_equal
                                                             low    = el_docnum-docnum
                                                           )
                         ).

      DELETE tl_cnslt_terminal_ax WHERE docnum NOT IN rl_docnum.

      APPEND LINES OF tl_cnslt_terminal_ax TO et_consulta_terminal.
      CLEAR: tl_cnslt_terminal_ax.


    ENDLOOP.

  ENDMETHOD.


  METHOD zif_controle_retorno_rfl~consulta_terminar_nfe.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZIF_CONTROLE_RETORNO_RFL~CONSULTA_TERMINAR_NFE                                       *
*& Chamado        : USER STORY 156221                                                                    *
*& Data           : 23/10/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 23/10/2024|DEVK9A1XAW |NSEGATIN       | Criação da Consulta Terminal de Embarque NFe                  *
*--------------------------------------------------------------------------------------------------------*

    DATA: ti_consulta_terminal TYPE zsdct_consulta_terminal.

    CONSTANTS: cl_z1 TYPE parvw VALUE 'Z1'.

    CLEAR: et_consulta_terminal[]. "<<<------"163355 - NMS"------>>>

    me->zif_controle_retorno_rfl~at_docnum = it_docnum.

    LOOP AT me->zif_controle_retorno_rfl~at_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>).
      APPEND INITIAL LINE TO ti_consulta_terminal ASSIGNING FIELD-SYMBOL(<fs_consulta_terminal>).
      <fs_consulta_terminal>-docnum = <fs_docnum>-docnum.
* Parceiros da Nota
      SELECT SINGLE docnum, parid FROM j_1bnfnad
        INTO @DATA(el_parceiro)
      WHERE docnum EQ @<fs_docnum>-docnum
        AND parvw  EQ @cl_z1.

      SELECT SINGLE docnum, novo_terminal, id_cc FROM zcarta_correcao
        INTO @DATA(el_carta_correcao)
        WHERE docnum        EQ @<fs_docnum>-docnum
         AND  novo_terminal NE @space.

      IF sy-subrc IS INITIAL.
        <fs_consulta_terminal>-lifnr = el_carta_correcao-novo_terminal.

      ELSE.
        <fs_consulta_terminal>-lifnr = el_parceiro-parid.

      ENDIF.

    ENDLOOP.

    IF NOT ti_consulta_terminal IS INITIAL.
      et_consulta_terminal                              = ti_consulta_terminal.
      me->zif_controle_retorno_rfl~at_consulta_terminal = ti_consulta_terminal.

    ENDIF.

  ENDMETHOD.


  METHOD zif_controle_retorno_rfl~reverte_retorno_1x1.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZIF_CONTROLE_RETORNO_RFL~REVERTE_RETORNO_1X1                                         *
*& Chamado        : USER STORY 157683                                                                    *
*& Data           : 13/11/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 13/11/2024|DEVK9A1XAW |NSEGATIN       | Reverter (estornar) o retorno de formação de lote.            *
*--------------------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Ranges                                                               *
*----------------------------------------------------------------------*
    DATA: lr_finalidades TYPE RANGE OF zfin_export,
          rl_final_ret_r TYPE RANGE OF zfin_export.
*----------------------------------------------------------------------*
* Vareables                                                            *
*----------------------------------------------------------------------*
    DATA: vl_vinc_flote_ownpro TYPE c.
*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
    CONSTANTS: cl_ieq TYPE char3 VALUE 'IEQ'.

*** Finalidades
    SELECT * FROM zsdt0359 INTO TABLE @DATA(tl_zsdt0359) WHERE excluido EQ @space.

    IF sy-subrc IS INITIAL.
      lr_finalidades = VALUE #( FOR el_zsdt0359 IN tl_zsdt0359 ( sign = cl_ieq(1) option = cl_ieq+1(2) low = el_zsdt0359-finalidade ) ).

    ENDIF.
*** Finalidades RFL - Fila NF Retorno de Recusa.
    SELECT * FROM zsdt0364 INTO TABLE @DATA(tl_zsdt0364) WHERE excluido EQ @space.

    IF sy-subrc IS INITIAL.
      rl_final_ret_r = VALUE #( FOR el_zsdt0364 IN tl_zsdt0364 ( sign = cl_ieq(1) option = cl_ieq+1(2) low = el_zsdt0364-finalidade ) ).

    ENDIF.

    SELECT * FROM zsdt_export INTO TABLE @DATA(tl_export) WHERE docnum EQ @i_docnum.

    CHECK sy-subrc IS INITIAL.

    SELECT * FROM zsdt_retlote
      INTO TABLE @DATA(tl_retlote)
      FOR ALL ENTRIES IN @tl_export
    WHERE docnum_ret EQ @tl_export-docnum.

    CHECK sy-subrc IS INITIAL.
    DATA(lt_retlote_aux) = tl_retlote.
    SORT lt_retlote_aux BY docnum.
    DELETE ADJACENT DUPLICATES FROM lt_retlote_aux COMPARING docnum.

    SELECT * FROM zsdtflote_flote
     INTO TABLE @DATA(tl_flote_flote)
     FOR ALL ENTRIES IN @lt_retlote_aux
    WHERE docnum EQ @lt_retlote_aux-docnum
      AND cancel EQ @abap_off.

    SELECT * FROM zsdtfl_flote_ref
      INTO TABLE @DATA(tl_fl_flote_ref)
      FOR ALL ENTRIES IN @lt_retlote_aux
    WHERE docnum_flote EQ @lt_retlote_aux-docnum
      AND cancel       EQ @abap_off.

    IF sy-subrc IS INITIAL.
      DATA(tl_fl_flote_ref2) = tl_fl_flote_ref.
      SORT tl_fl_flote_ref2 BY docnum_flote docnum_eprod.
      DELETE ADJACENT DUPLICATES FROM tl_fl_flote_ref2 COMPARING docnum_flote docnum_eprod.

      SELECT * FROM zsdtvinc_p_flote
        INTO TABLE @DATA(tl_vinc_p_flote_docref)
       FOR ALL ENTRIES IN @tl_fl_flote_ref2
      WHERE docnum_flote EQ @tl_fl_flote_ref2-docnum_flote
        AND docnum_eprod EQ @tl_fl_flote_ref2-docnum_eprod.

      IF sy-subrc IS INITIAL.
        SORT tl_vinc_p_flote_docref BY docnum_flote docnum_eprod cancel ASCENDING id_vinc DESCENDING.
        DATA(tl_vinc_p_flote_docref2) = tl_vinc_p_flote_docref.
        SORT tl_vinc_p_flote_docref2 BY docnum_flote docnum_ref.
        DELETE ADJACENT DUPLICATES FROM tl_vinc_p_flote_docref2 COMPARING docnum_flote docnum_ref.

        SELECT * FROM zsdtvinc_p_flote
          INTO TABLE @DATA(tl_vinc_p_flote_virt)
         FOR ALL ENTRIES IN @tl_vinc_p_flote_docref2
        WHERE docnum_flote EQ @tl_vinc_p_flote_docref2-docnum_flote
          AND docnum_eprod EQ @tl_vinc_p_flote_docref2-docnum_ref.

        IF sy-subrc IS INITIAL.
          SORT tl_vinc_p_flote_virt BY docnum_flote docnum_ref cancel ASCENDING id_vinc DESCENDING.

        ENDIF.

      ENDIF.

    ENDIF.
* Verifica se a Nota é de Produção Própria.
    SELECT docnum, ownpro
      FROM j_1bnflin
      INTO TABLE @DATA(tl_ownpro)
      FOR ALL ENTRIES IN @lt_retlote_aux
    WHERE docnum EQ @lt_retlote_aux-docnum.

    DATA: tl_vinc_p_flote TYPE zsdtvinc_p_flote_t.

    SELECT * FROM zsdtvinc_p_flote
      INTO TABLE tl_vinc_p_flote
     FOR ALL ENTRIES IN lt_retlote_aux
    WHERE docnum_flote EQ lt_retlote_aux-docnum.

    IF sy-subrc IS INITIAL.
      DATA(tl_vinc_p_flote_all) = tl_vinc_p_flote.
      SORT tl_vinc_p_flote_all BY docnum_flote docnum_eprod ASCENDING id_vinc DESCENDING docnum_ref ASCENDING.

      SELECT * FROM zsdtprod_flote
        INTO TABLE @DATA(tl_prod_flote)
        FOR ALL ENTRIES IN @tl_vinc_p_flote
      WHERE docnum EQ @tl_vinc_p_flote-docnum_eprod
        AND cancel EQ @abap_off.

      SELECT * FROM zsdtflote_flote
       APPENDING TABLE @DATA(tl_flote_flote_vit)
        FOR ALL ENTRIES IN @tl_vinc_p_flote
      WHERE docnum EQ @tl_vinc_p_flote-docnum_eprod
        AND cancel EQ @abap_off.

      IF sy-subrc IS INITIAL.
        SELECT * FROM zsdtfl_flote_ref
          INTO TABLE @DATA(tl_fl_flote_ref_vit)
          FOR ALL ENTRIES IN @tl_flote_flote_vit
        WHERE docnum_flote EQ @tl_flote_flote_vit-docnum
          AND cancel       EQ @abap_off.

      ENDIF.

    ENDIF.

    DATA(tl_flote_flote_up)  = tl_flote_flote.
    DATA(tl_fl_flote_ref_up) = tl_fl_flote_ref.
    DATA(tl_vinc_p_flote_up) = tl_vinc_p_flote.
    DATA(tl_prod_flote_up)   = tl_prod_flote.
    CLEAR: tl_flote_flote_up, tl_fl_flote_ref_up, tl_vinc_p_flote_up, tl_prod_flote_up.

    LOOP AT tl_export INTO DATA(el_export).
      LOOP AT tl_retlote INTO DATA(el_retlote) WHERE docnum_ret EQ el_export-docnum.
        DATA(vl_quant_vinc) = el_retlote-quant_vinc.
        READ TABLE tl_ownpro INTO DATA(el_ownpro) WITH KEY docnum = el_retlote-docnum.
* Verifica o Tipo da Finalidade de Exportação
        IF     el_export-finalidade IN lr_finalidades. "Finalidades de retorno
          LOOP AT tl_flote_flote INTO DATA(el_flote_flote) WHERE docnum EQ el_retlote-docnum.

            IF vl_quant_vinc LE el_flote_flote-saldo_disponivel.
              DATA(lv_subtraendo) = vl_quant_vinc.

            ELSE.
              lv_subtraendo = el_flote_flote-saldo_disponivel.

            ENDIF.
* Marca linha como cancelado ZSDTFLOTE_FLOTE.
            MOVE: abap_on  TO el_flote_flote-cancel,
                  sy-uname TO el_flote_flote-us_cancel,
                  sy-datlo TO el_flote_flote-dt_cancel,
                  sy-timlo TO el_flote_flote-hr_cancel.

            APPEND el_flote_flote TO tl_flote_flote_up.
* Verifica so o documento do Flote Flote não é igual ao documento de retornado(RETLOTE) ou
            IF el_retlote-docnum NE el_flote_flote-docnum.
* Cria uma nova linha ZSDTFLOTE_FLOTE.
              CLEAR: el_flote_flote-cancel, el_flote_flote-us_cancel, el_flote_flote-dt_cancel, el_flote_flote-hr_cancel.
              el_flote_flote-saldo_vinc       = el_flote_flote-saldo_vinc + lv_subtraendo.
              el_flote_flote-saldo_disponivel = el_flote_flote-saldo_disponivel - lv_subtraendo.
              el_flote_flote-us_criacao       = sy-uname.
              el_flote_flote-dt_criacao       = sy-datlo.
              el_flote_flote-hr_criacao       = sy-timlo.

              IF el_flote_flote-saldo_disponivel IS INITIAL.
                el_flote_flote-saldo_nao_disponivel = abap_on.

              ELSE.
                el_flote_flote-saldo_nao_disponivel = abap_off.

              ENDIF.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
              PERFORM zf_number_get_next IN PROGRAM zsdr0012 USING    '01'
                                                                      'ZSEQFLOTE'
                                                             CHANGING el_flote_flote-seq.

              APPEND el_flote_flote TO tl_flote_flote_up.

            ENDIF.

            vl_quant_vinc = vl_quant_vinc - lv_subtraendo.

            IF vl_quant_vinc IS INITIAL.
              EXIT.

            ENDIF.

          ENDLOOP.

          vl_quant_vinc = el_retlote-quant_vinc.

          LOOP AT tl_fl_flote_ref INTO DATA(el_fl_flote_ref) WHERE docnum_flote EQ el_retlote-docnum.
            IF vl_quant_vinc LE el_fl_flote_ref-saldo_disponivel.
              lv_subtraendo = vl_quant_vinc.

            ELSE.
              lv_subtraendo = el_fl_flote_ref-saldo_disponivel.

            ENDIF.
* Marca linha como cancelado ZSDTFL_FLOTE_REF.
            MOVE: abap_on  TO el_fl_flote_ref-cancel,
                  sy-uname TO el_fl_flote_ref-us_cancel,
                  sy-datlo TO el_fl_flote_ref-dt_cancel,
                  sy-timlo TO el_fl_flote_ref-hr_cancel.

            APPEND el_fl_flote_ref TO tl_fl_flote_ref_up.
* Verifica so o documento do Flote Flote não é igual ao documento de retornado(RETLOTE).
            IF el_retlote-docnum NE el_fl_flote_ref-docnum_flote.
* Cria uma nova linha ZSDTFL_FLOTE_REF.
              CLEAR: el_fl_flote_ref-cancel, el_fl_flote_ref-us_cancel, el_fl_flote_ref-dt_cancel, el_fl_flote_ref-hr_cancel.
              el_fl_flote_ref-qtd_vinc         = el_fl_flote_ref-qtd_vinc + lv_subtraendo.
              el_fl_flote_ref-saldo_disponivel = el_fl_flote_ref-saldo_disponivel - lv_subtraendo.
              el_fl_flote_ref-us_criacao       = sy-uname.
              el_fl_flote_ref-dt_criacao       = sy-datlo.
              el_fl_flote_ref-hr_criacao       = sy-timlo.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
              PERFORM zf_number_get_next IN PROGRAM zsdr0012 USING    '01'
                                                                      'ZSEQFLREF'
                                                             CHANGING el_fl_flote_ref-seq.

              APPEND el_fl_flote_ref TO tl_fl_flote_ref_up.

            ENDIF.
* Verifica so o documento do Flote Flote é igual ao documento de retornado(RETLOTE).
            IF el_retlote-docnum EQ el_fl_flote_ref-docnum_flote.
*** Criação do Registro de Documento de Referência na tabela de vínculos.
              READ TABLE tl_vinc_p_flote_docref INTO DATA(el_vinc_p_flote_docref) WITH KEY docnum_flote = el_fl_flote_ref-docnum_flote
                                                                                           docnum_eprod = el_fl_flote_ref-docnum_eprod
                                                                                           docnum_ref   = '0000000000'
                                                                                           cancel       = abap_off.

              IF sy-subrc IS INITIAL.
* Marca linha como cancelado ZSDTVINC_P_FLOTE.
                MOVE: abap_on  TO el_vinc_p_flote_docref-cancel,
                      sy-uname TO el_vinc_p_flote_docref-us_cancel,
                      sy-datlo TO el_vinc_p_flote_docref-dt_cancel,
                      sy-timlo TO el_vinc_p_flote_docref-hr_cancel.

                APPEND el_vinc_p_flote_docref TO tl_vinc_p_flote_up.

                el_vinc_p_flote_docref-qtd_vinc = el_vinc_p_flote_docref-qtd_vinc + lv_subtraendo.

              ELSE.
                READ TABLE tl_vinc_p_flote_docref INTO el_vinc_p_flote_docref WITH KEY docnum_flote = el_fl_flote_ref-docnum_flote
                                                                                       docnum_eprod = el_fl_flote_ref-docnum_eprod
                                                                                       docnum_ref   = '0000000000'
                                                                                       cancel       = abap_on.

                IF sy-subrc IS INITIAL.
                  el_vinc_p_flote_docref-qtd_vinc = lv_subtraendo.

                ENDIF.

              ENDIF.

            ELSE.
*** Criação do Registro de Documento de Referência na tabela de vínculos.
              READ TABLE tl_vinc_p_flote_docref INTO el_vinc_p_flote_docref WITH KEY docnum_flote = el_fl_flote_ref-docnum_flote
                                                                                     docnum_eprod = el_fl_flote_ref-docnum_eprod
                                                                                     cancel       = abap_off.

              IF sy-subrc IS INITIAL.
* Marca linha como cancelado ZSDTVINC_P_FLOTE.
                MOVE: abap_on  TO el_vinc_p_flote_docref-cancel,
                      sy-uname TO el_vinc_p_flote_docref-us_cancel,
                      sy-datlo TO el_vinc_p_flote_docref-dt_cancel,
                      sy-timlo TO el_vinc_p_flote_docref-hr_cancel.

                APPEND el_vinc_p_flote_docref TO tl_vinc_p_flote_up.

                el_vinc_p_flote_docref-qtd_vinc = el_vinc_p_flote_docref-qtd_vinc + lv_subtraendo.

              ELSE.
                READ TABLE tl_vinc_p_flote_docref INTO el_vinc_p_flote_docref WITH KEY docnum_flote = el_fl_flote_ref-docnum_flote
                                                                                       docnum_eprod = el_fl_flote_ref-docnum_eprod
                                                                                       cancel       = abap_on.

                IF sy-subrc IS INITIAL.
                  el_vinc_p_flote_docref-qtd_vinc = lv_subtraendo.

                ENDIF.

              ENDIF.

            ENDIF.

            IF sy-subrc IS INITIAL.
* Cria uma nova linha ZSDTVINC_P_FLOTE.
              CLEAR: el_vinc_p_flote_docref-cancel, el_vinc_p_flote_docref-us_cancel, el_vinc_p_flote_docref-dt_cancel, el_vinc_p_flote_docref-hr_cancel.
              el_vinc_p_flote_docref-us_criacao = sy-uname.
              el_vinc_p_flote_docref-dt_criacao = sy-datlo.
              el_vinc_p_flote_docref-hr_criacao = sy-timlo.
* Busca o último valor do campo ID_VINC da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
              SELECT SINGLE MAX( id_vinc ) FROM zsdtvinc_p_flote
                INTO el_vinc_p_flote_docref-id_vinc
              WHERE docnum_flote EQ el_vinc_p_flote_docref-docnum_flote
                AND docnum_eprod EQ el_vinc_p_flote_docref-docnum_eprod
                AND docnum_ref   EQ el_vinc_p_flote_docref-docnum_ref.

              IF sy-subrc IS INITIAL.
                ADD 1 TO el_vinc_p_flote_docref-id_vinc.

              ELSE.
                el_vinc_p_flote_docref-id_vinc = 1.

              ENDIF.

              APPEND el_vinc_p_flote_docref TO tl_vinc_p_flote_up.

            ENDIF.
*** Criação do Linha do Registro Virtual na tabela de vínculos.
            READ TABLE tl_vinc_p_flote_virt INTO DATA(el_vinc_p_flote_virt) WITH KEY docnum_flote = el_vinc_p_flote_docref-docnum_flote
                                                                                     docnum_eprod = el_vinc_p_flote_docref-docnum_ref
                                                                                     cancel       = abap_off.

            IF sy-subrc IS INITIAL.
* Marca linha como cancelado ZSDTVINC_P_FLOTE.
              MOVE: abap_on  TO el_vinc_p_flote_virt-cancel,
                    sy-uname TO el_vinc_p_flote_virt-us_cancel,
                    sy-datlo TO el_vinc_p_flote_virt-dt_cancel,
                    sy-timlo TO el_vinc_p_flote_virt-hr_cancel.

              APPEND el_vinc_p_flote_virt TO tl_vinc_p_flote_up.

              el_vinc_p_flote_virt-qtd_vinc = el_vinc_p_flote_virt-qtd_vinc + lv_subtraendo.

            ELSE.
              READ TABLE tl_vinc_p_flote_virt INTO el_vinc_p_flote_virt WITH KEY docnum_flote = el_vinc_p_flote_docref-docnum_flote
                                                                                 docnum_eprod = el_vinc_p_flote_docref-docnum_ref
                                                                                 cancel       = abap_on.

              IF sy-subrc IS INITIAL.
                el_vinc_p_flote_virt-qtd_vinc = lv_subtraendo.

              ENDIF.

            ENDIF.

            IF sy-subrc IS INITIAL.
* Cria uma nova linha ZSDTVINC_P_FLOTE.
              CLEAR: el_vinc_p_flote_virt-cancel, el_vinc_p_flote_virt-us_cancel, el_vinc_p_flote_virt-dt_cancel, el_vinc_p_flote_virt-hr_cancel.
              el_vinc_p_flote_virt-us_criacao = sy-uname.
              el_vinc_p_flote_virt-dt_criacao = sy-datlo.
              el_vinc_p_flote_virt-hr_criacao = sy-timlo.
* Busca o último valor do campo ID_VINC da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
              SELECT SINGLE MAX( id_vinc ) FROM zsdtvinc_p_flote
                INTO el_vinc_p_flote_virt-id_vinc
              WHERE docnum_flote EQ el_vinc_p_flote_virt-docnum_flote
                AND docnum_eprod EQ el_vinc_p_flote_virt-docnum_eprod
                AND docnum_ref   EQ el_vinc_p_flote_virt-docnum_ref.

              IF sy-subrc IS INITIAL.
                ADD 1 TO el_vinc_p_flote_virt-id_vinc.

              ELSE.
                el_vinc_p_flote_virt-id_vinc = 1.

              ENDIF.

              APPEND el_vinc_p_flote_virt TO tl_vinc_p_flote_up.

            ENDIF.

            vl_quant_vinc = vl_quant_vinc - lv_subtraendo.

            IF vl_quant_vinc IS INITIAL.
              EXIT.

            ENDIF.

          ENDLOOP.
*** Processamento Virtual para quando se tem saldo ainda.
          IF NOT vl_quant_vinc IS INITIAL.
            DATA(vl_quant_vinc2) = vl_quant_vinc.
* Tbl. lógica - Vinculo formação de lote para Reverter Retorno
            zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->vinc_flote_reverte_retorno( EXPORTING it_vinc_p_flote_in  = tl_vinc_p_flote
                                                                                                            IMPORTING et_vinc_p_flote_out = tl_vinc_p_flote ).

            LOOP AT tl_vinc_p_flote INTO DATA(el_vinc_p_flote) WHERE docnum_flote EQ el_retlote-docnum
                                                                 AND docnum_ref   IS INITIAL
                                                                 AND vinc_virtual EQ abap_on.

              LOOP AT tl_flote_flote_vit INTO el_flote_flote WHERE docnum EQ el_vinc_p_flote-docnum_eprod.
                DATA(vl_tabix) = sy-tabix.

                IF vl_quant_vinc LE el_flote_flote-saldo_disponivel.
                  lv_subtraendo = vl_quant_vinc.

                ELSE.
                  lv_subtraendo = el_flote_flote-saldo_disponivel.

                ENDIF.
* Verifica se já existe na TI de atualização de uma linha ZSDTFLOTE_FLOTE já marcada para cancelar.
                READ TABLE tl_flote_flote_up TRANSPORTING NO FIELDS WITH KEY docnum = el_flote_flote-docnum
                                                                             itmnum = el_flote_flote-itmnum
                                                                             seq    = el_flote_flote-seq
                                                                             cancel = abap_on.

                IF NOT sy-subrc IS INITIAL.
* Marca linha como cancelado ZSDTFLOTE_FLOTE.
                  MOVE: abap_on  TO el_flote_flote-cancel,
                        sy-uname TO el_flote_flote-us_cancel,
                        sy-datlo TO el_flote_flote-dt_cancel,
                        sy-timlo TO el_flote_flote-hr_cancel.

                  APPEND el_flote_flote TO tl_flote_flote_up.

                ENDIF.
* Verifica se já existe na TI de atualização de uma nova linha ZSDTFLOTE_FLOTE.
                READ TABLE tl_flote_flote_up ASSIGNING FIELD-SYMBOL(<fs_flote_flote_up>) WITH KEY docnum = el_flote_flote-docnum
                                                                                                  itmnum = el_flote_flote-itmnum
                                                                                                  cancel = abap_off.

                IF sy-subrc IS INITIAL.
* Ajusta o registro já existente na TI de atualização de uma nova linha ZSDTFLOTE_FLOTE.
                  CLEAR: <fs_flote_flote_up>-cancel, <fs_flote_flote_up>-us_cancel, <fs_flote_flote_up>-dt_cancel, <fs_flote_flote_up>-hr_cancel.
                  <fs_flote_flote_up>-saldo_vinc       = el_flote_flote-saldo_vinc + lv_subtraendo.
                  <fs_flote_flote_up>-saldo_disponivel = el_flote_flote-saldo_disponivel - lv_subtraendo.
                  <fs_flote_flote_up>-us_criacao       = sy-uname.
                  <fs_flote_flote_up>-dt_criacao       = sy-datlo.
                  <fs_flote_flote_up>-hr_criacao       = sy-timlo.

                  IF <fs_flote_flote_up>-saldo_disponivel IS INITIAL.
                    <fs_flote_flote_up>-saldo_nao_disponivel = abap_on.

                  ELSE.
                    <fs_flote_flote_up>-saldo_nao_disponivel = abap_off.

                  ENDIF.

                  MODIFY tl_flote_flote_vit FROM <fs_flote_flote_up> INDEX vl_tabix TRANSPORTING saldo_vinc saldo_disponivel.

                ELSE.
* Cria uma nova linha ZSDTFLOTE_FLOTE.
                  CLEAR: el_flote_flote-cancel, el_flote_flote-us_cancel, el_flote_flote-dt_cancel, el_flote_flote-hr_cancel.
                  el_flote_flote-saldo_vinc       = el_flote_flote-saldo_vinc + lv_subtraendo.
                  el_flote_flote-saldo_disponivel = el_flote_flote-saldo_disponivel - lv_subtraendo.
                  el_flote_flote-us_criacao       = sy-uname.
                  el_flote_flote-dt_criacao       = sy-datlo.
                  el_flote_flote-hr_criacao       = sy-timlo.

                  IF el_flote_flote-saldo_disponivel IS INITIAL.
                    el_flote_flote-saldo_nao_disponivel = abap_on.

                  ELSE.
                    el_flote_flote-saldo_nao_disponivel = abap_off.

                  ENDIF.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
                  PERFORM zf_number_get_next IN PROGRAM zsdr0012 USING    '01'
                                                                          'ZSEQFLOTE'
                                                                 CHANGING el_flote_flote-seq.

                  APPEND el_flote_flote TO tl_flote_flote_up.
                  MODIFY tl_flote_flote_vit FROM el_flote_flote INDEX vl_tabix TRANSPORTING saldo_vinc saldo_disponivel.

                ENDIF.

                vl_quant_vinc = vl_quant_vinc - lv_subtraendo.
* Verifica se é para gerar o vinculo virtual da NF de Produto Próprio.
                IF NOT vl_vinc_flote_ownpro IS INITIAL.
                  vl_quant_vinc = vl_quant_vinc2.

                ENDIF.

                IF vl_quant_vinc IS INITIAL.
                  vl_quant_vinc = vl_quant_vinc2.

                  LOOP AT tl_fl_flote_ref_vit INTO el_fl_flote_ref WHERE docnum_flote EQ el_vinc_p_flote-docnum_eprod.
                    IF el_fl_flote_ref-saldo_disponivel IS INITIAL.
                      CONTINUE.

                    ENDIF.

                    IF vl_quant_vinc LE el_fl_flote_ref-saldo_disponivel.
                      lv_subtraendo = vl_quant_vinc.

                    ELSE.
                      lv_subtraendo = el_fl_flote_ref-saldo_disponivel.

                    ENDIF.
* Marca linha como cancelado ZSDTFL_FLOTE_REF.
                    MOVE: abap_on  TO el_fl_flote_ref-cancel,
                          sy-uname TO el_fl_flote_ref-us_cancel,
                          sy-datlo TO el_fl_flote_ref-dt_cancel,
                          sy-timlo TO el_fl_flote_ref-hr_cancel.

                    APPEND el_fl_flote_ref TO tl_fl_flote_ref_up.
* Cria uma nova linha ZSDTFL_FLOTE_REF.
                    CLEAR: el_fl_flote_ref-cancel, el_fl_flote_ref-us_cancel, el_fl_flote_ref-dt_cancel, el_fl_flote_ref-hr_cancel.
                    el_fl_flote_ref-qtd_vinc         = el_fl_flote_ref-qtd_vinc + lv_subtraendo.
                    el_fl_flote_ref-saldo_disponivel = el_fl_flote_ref-saldo_disponivel - lv_subtraendo.
                    el_fl_flote_ref-us_criacao       = sy-uname.
                    el_fl_flote_ref-dt_criacao       = sy-datlo.
                    el_fl_flote_ref-hr_criacao       = sy-timlo.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
                    PERFORM zf_number_get_next IN PROGRAM zsdr0012 USING    '01'
                                                                            'ZSEQFLREF'
                                                                   CHANGING el_fl_flote_ref-seq.

                    APPEND el_fl_flote_ref TO tl_fl_flote_ref_up.
*** Criação do Registro de Documento de Referência na tabela de vínculos.
                    READ TABLE tl_vinc_p_flote_all INTO el_vinc_p_flote_docref WITH KEY docnum_flote = el_retlote-docnum
                                                                                        docnum_eprod = el_fl_flote_ref-docnum_eprod
                                                                                        docnum_ref   = el_fl_flote_ref-docnum_flote
                                                                                        cancel       = abap_off.

                    IF sy-subrc IS INITIAL.
* Marca linha como cancelado ZSDTVINC_P_FLOTE.
                      MOVE: abap_on  TO el_vinc_p_flote_docref-cancel,
                            sy-uname TO el_vinc_p_flote_docref-us_cancel,
                            sy-datlo TO el_vinc_p_flote_docref-dt_cancel,
                            sy-timlo TO el_vinc_p_flote_docref-hr_cancel.

                      APPEND el_vinc_p_flote_docref TO tl_vinc_p_flote_up.

                      el_vinc_p_flote_docref-qtd_vinc = el_vinc_p_flote_docref-qtd_vinc + lv_subtraendo.

                    ELSE.
                      READ TABLE tl_vinc_p_flote_all INTO el_vinc_p_flote_docref WITH KEY docnum_flote = el_retlote-docnum
                                                                                          docnum_eprod = el_fl_flote_ref-docnum_eprod
                                                                                          docnum_ref   = el_fl_flote_ref-docnum_flote
                                                                                          cancel       = abap_on.

                      IF sy-subrc IS INITIAL.
                        el_vinc_p_flote_docref-qtd_vinc = lv_subtraendo.

                      ENDIF.

                    ENDIF.

                    IF sy-subrc IS INITIAL.
* Cria uma nova linha ZSDTVINC_P_FLOTE.
                      CLEAR: el_vinc_p_flote_docref-cancel, el_vinc_p_flote_docref-us_cancel, el_vinc_p_flote_docref-dt_cancel, el_vinc_p_flote_docref-hr_cancel.
                      el_vinc_p_flote_docref-us_criacao = sy-uname.
                      el_vinc_p_flote_docref-dt_criacao = sy-datlo.
                      el_vinc_p_flote_docref-hr_criacao = sy-timlo.
* Busca o último valor do campo ID_VINC da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
                      SELECT SINGLE MAX( id_vinc ) FROM zsdtvinc_p_flote
                        INTO el_vinc_p_flote_docref-id_vinc
                      WHERE docnum_flote EQ el_vinc_p_flote_docref-docnum_flote
                        AND docnum_eprod EQ el_vinc_p_flote_docref-docnum_eprod
                        AND docnum_ref   EQ el_vinc_p_flote_docref-docnum_ref.

                      IF sy-subrc IS INITIAL.
                        ADD 1 TO el_vinc_p_flote_docref-id_vinc.

                      ELSE.
                        el_vinc_p_flote_docref-id_vinc = 1.

                      ENDIF.

                      APPEND el_vinc_p_flote_docref TO tl_vinc_p_flote_up.
* Monta o registro virtual de referência.
                      READ TABLE tl_vinc_p_flote_up INTO DATA(el_vinc_p_flote_up) WITH KEY docnum_flote = el_vinc_p_flote_docref-docnum_flote
                                                                                           docnum_eprod = el_vinc_p_flote_docref-docnum_ref
                                                                                           docnum_ref   = '0000000000'
                                                                                           vinc_virtual = abap_on
                                                                                           cancel       = abap_off.

                      IF sy-subrc IS INITIAL.
                        el_vinc_p_flote_up-qtd_vinc   = el_vinc_p_flote_up-qtd_vinc + lv_subtraendo.
                        el_vinc_p_flote_up-us_criacao = sy-uname.
                        el_vinc_p_flote_up-dt_criacao = sy-datlo.
                        el_vinc_p_flote_up-hr_criacao = sy-timlo.

                        MODIFY tl_vinc_p_flote_up FROM el_vinc_p_flote_up INDEX sy-tabix TRANSPORTING qtd_vinc us_criacao dt_criacao hr_criacao.

                      ELSE.
* Busca o último valor do campo ID_VINC da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
                        SELECT SINGLE MAX( id_vinc ) FROM zsdtvinc_p_flote
                          INTO el_vinc_p_flote_up-id_vinc
                        WHERE docnum_flote EQ el_vinc_p_flote_docref-docnum_flote
                          AND docnum_eprod EQ el_vinc_p_flote_docref-docnum_ref
                          AND docnum_ref   EQ '0000000000'
                          AND vinc_virtual EQ abap_on.

                        IF sy-subrc IS INITIAL.
* Busca o registro da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
                          SELECT SINGLE * FROM zsdtvinc_p_flote
                            INTO el_vinc_p_flote_up
                          WHERE docnum_flote EQ el_vinc_p_flote_docref-docnum_flote
                            AND docnum_eprod EQ el_vinc_p_flote_docref-docnum_ref
                            AND id_vinc      EQ el_vinc_p_flote_up-id_vinc.

                          IF sy-subrc IS INITIAL.
                            IF el_vinc_p_flote_up-cancel IS INITIAL.
* Marca linha como cancelado ZSDTVINC_P_FLOTE.
                              MOVE: abap_on  TO el_vinc_p_flote_up-cancel,
                                    sy-uname TO el_vinc_p_flote_up-us_cancel,
                                    sy-datlo TO el_vinc_p_flote_up-dt_cancel,
                                    sy-timlo TO el_vinc_p_flote_up-hr_cancel.

                              APPEND el_vinc_p_flote_up TO tl_vinc_p_flote_up.

                              el_vinc_p_flote_up-qtd_vinc = el_vinc_p_flote_up-qtd_vinc + lv_subtraendo.

                            ELSE.
                              el_vinc_p_flote_up-qtd_vinc = lv_subtraendo.

                            ENDIF.
* Cria uma nova linha ZSDTVINC_P_FLOTE.
                            ADD 1 TO el_vinc_p_flote_up-id_vinc.
                            CLEAR: el_vinc_p_flote_up-cancel, el_vinc_p_flote_up-us_cancel, el_vinc_p_flote_up-dt_cancel, el_vinc_p_flote_up-hr_cancel.
                            el_vinc_p_flote_up-us_criacao = sy-uname.
                            el_vinc_p_flote_up-dt_criacao = sy-datlo.
                            el_vinc_p_flote_up-hr_criacao = sy-timlo.

                            APPEND el_vinc_p_flote_up TO tl_vinc_p_flote_up.

                          ENDIF.

                        ENDIF.

                      ENDIF.

                    ENDIF.

                    vl_quant_vinc = vl_quant_vinc - lv_subtraendo.

                    IF vl_quant_vinc IS INITIAL.
                      EXIT.

                    ENDIF.

                  ENDLOOP.

                ELSE.
                  IF NOT el_ownpro-ownpro IS INITIAL.
*** Criação do Registro de Documento de Produto Próprio na tabela de vínculos.
                    IF el_vinc_p_flote-cancel IS INITIAL.
* Marca linha como cancelado ZSDTVINC_P_FLOTE.
                      MOVE: abap_on  TO el_vinc_p_flote-cancel,
                            sy-uname TO el_vinc_p_flote-us_cancel,
                            sy-datlo TO el_vinc_p_flote-dt_cancel,
                            sy-timlo TO el_vinc_p_flote-hr_cancel.

                      APPEND el_vinc_p_flote TO tl_vinc_p_flote_up.

                      el_vinc_p_flote-qtd_vinc = el_vinc_p_flote-qtd_vinc + lv_subtraendo.

                    ELSE.
                      el_vinc_p_flote-qtd_vinc = lv_subtraendo.

                    ENDIF.
* Cria uma nova linha ZSDTVINC_P_FLOTE.
                    CLEAR: el_vinc_p_flote-cancel, el_vinc_p_flote-us_cancel, el_vinc_p_flote-dt_cancel, el_vinc_p_flote-hr_cancel.
                    el_vinc_p_flote-us_criacao = sy-uname.
                    el_vinc_p_flote-dt_criacao = sy-datlo.
                    el_vinc_p_flote-hr_criacao = sy-timlo.
* Busca o último valor do campo ID_VINC da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
                    SELECT SINGLE MAX( id_vinc ) FROM zsdtvinc_p_flote
                      INTO el_vinc_p_flote-id_vinc
                    WHERE docnum_flote EQ el_vinc_p_flote-docnum_flote
                      AND docnum_eprod EQ el_vinc_p_flote-docnum_eprod
                      AND docnum_ref   EQ el_vinc_p_flote-docnum_ref.

                    IF sy-subrc IS INITIAL.
                      ADD 1 TO el_vinc_p_flote-id_vinc.

                    ELSE.
                      el_vinc_p_flote-id_vinc = 1.

                    ENDIF.

                    APPEND el_vinc_p_flote TO tl_vinc_p_flote_up.
* Verifica se não é para gerar o vinculo virtual da NF de Produto Próprio.
                    IF vl_vinc_flote_ownpro IS INITIAL.
                      vl_quant_vinc2 = vl_quant_vinc2 - lv_subtraendo.
                      vl_quant_vinc  = vl_quant_vinc  - vl_quant_vinc2.

                    ELSE.
                      vl_quant_vinc  = vl_quant_vinc  - vl_quant_vinc2.
                      vl_quant_vinc2 = vl_quant_vinc2 - lv_subtraendo.
                      CLEAR: lv_subtraendo, vl_vinc_flote_ownpro.

                    ENDIF.

                  ENDIF.

                ENDIF.

                IF vl_quant_vinc IS INITIAL.
                  IF vl_quant_vinc2 IS INITIAL.
                    EXIT.

                  ELSE.
                    vl_quant_vinc = vl_quant_vinc2.
                    vl_vinc_flote_ownpro = abap_on.
                    CONTINUE.

                  ENDIF.

                ELSE.
                  IF NOT el_ownpro-ownpro IS INITIAL.
*** Criação do Registro de Documento de Referência na tabela de vínculos.
                    READ TABLE tl_vinc_p_flote_all INTO el_vinc_p_flote WITH KEY docnum_flote = el_vinc_p_flote-docnum_flote
                                                                                 docnum_eprod = el_vinc_p_flote-docnum_eprod
                                                                                 docnum_ref   = el_vinc_p_flote-docnum_ref
                                                                                 cancel       = abap_off.

                    IF sy-subrc IS INITIAL.
* Marca linha como cancelado ZSDTVINC_P_FLOTE.
                      MOVE: abap_on  TO el_vinc_p_flote-cancel,
                            sy-uname TO el_vinc_p_flote-us_cancel,
                            sy-datlo TO el_vinc_p_flote-dt_cancel,
                            sy-timlo TO el_vinc_p_flote-hr_cancel.

                      APPEND el_vinc_p_flote TO tl_vinc_p_flote_up.

                      el_vinc_p_flote-qtd_vinc = el_vinc_p_flote-qtd_vinc + lv_subtraendo.

                    ELSE.
                      READ TABLE tl_vinc_p_flote_all INTO el_vinc_p_flote WITH KEY docnum_flote = el_vinc_p_flote-docnum_flote
                                                                                   docnum_eprod = el_vinc_p_flote-docnum_eprod
                                                                                   docnum_ref   = el_vinc_p_flote-docnum_ref
                                                                                   cancel       = abap_on.

                      IF sy-subrc IS INITIAL.
                        el_vinc_p_flote-qtd_vinc = lv_subtraendo.

                      ENDIF.

                    ENDIF.

                    IF sy-subrc IS INITIAL.
                      READ TABLE tl_vinc_p_flote_up ASSIGNING FIELD-SYMBOL(<fs_vinc_p_flote_up>) WITH KEY docnum_flote = el_vinc_p_flote-docnum_flote
                                                                                                          docnum_eprod = el_vinc_p_flote-docnum_eprod
                                                                                                          docnum_ref   = el_vinc_p_flote-docnum_ref
                                                                                                          cancel       = abap_off.

                      IF sy-subrc IS INITIAL.
                        ADD el_vinc_p_flote-qtd_vinc TO <fs_vinc_p_flote_up>-qtd_vinc.
* Atualiza a linha de uma nova linha ZSDTVINC_P_FLOTE.
                        CLEAR: <fs_vinc_p_flote_up>-cancel, <fs_vinc_p_flote_up>-us_cancel, <fs_vinc_p_flote_up>-dt_cancel, <fs_vinc_p_flote_up>-hr_cancel.
                        <fs_vinc_p_flote_up>-us_criacao = sy-uname.
                        <fs_vinc_p_flote_up>-dt_criacao = sy-datlo.
                        <fs_vinc_p_flote_up>-hr_criacao = sy-timlo.

                      ELSE.
* Cria uma nova linha ZSDTVINC_P_FLOTE.
                        CLEAR: el_vinc_p_flote-cancel, el_vinc_p_flote-us_cancel, el_vinc_p_flote-dt_cancel, el_vinc_p_flote-hr_cancel.
                        el_vinc_p_flote-us_criacao = sy-uname.
                        el_vinc_p_flote-dt_criacao = sy-datlo.
                        el_vinc_p_flote-hr_criacao = sy-timlo.
* Busca o último valor do campo ID_VINC da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
                        SELECT SINGLE MAX( id_vinc ) FROM zsdtvinc_p_flote
                          INTO el_vinc_p_flote-id_vinc
                        WHERE docnum_flote EQ el_vinc_p_flote-docnum_flote
                          AND docnum_eprod EQ el_vinc_p_flote-docnum_eprod
                          AND docnum_ref   EQ el_vinc_p_flote-docnum_ref.

                        IF sy-subrc IS INITIAL.
                          ADD 1 TO el_vinc_p_flote-id_vinc.

                        ELSE.
                          el_vinc_p_flote-id_vinc = 1.

                        ENDIF.

                        APPEND el_vinc_p_flote TO tl_vinc_p_flote_up.

                      ENDIF.

                      vl_quant_vinc  = vl_quant_vinc  - lv_subtraendo.

                    ENDIF.

                  ENDIF.

                  IF vl_quant_vinc IS INITIAL.
                    EXIT.

                  ENDIF.

                ENDIF.

              ENDLOOP.

              IF vl_quant_vinc IS INITIAL.
                EXIT.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ELSEIF el_export-finalidade IN rl_final_ret_r. "Finalidades de retorno de recusa
* Tbl. lógica - Vinculo formação de lote para Reverter Retorno
          zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->vinc_flote_reverte_retorno( EXPORTING it_vinc_p_flote_in  = tl_vinc_p_flote
                                                                                                          IMPORTING et_vinc_p_flote_out = tl_vinc_p_flote ).

          LOOP AT tl_vinc_p_flote INTO el_vinc_p_flote WHERE docnum_flote EQ el_retlote-docnum
                                                          AND docnum_ref  IS INITIAL.
            IF vl_quant_vinc LE el_vinc_p_flote-qtd_vinc.
              lv_subtraendo = vl_quant_vinc.

            ELSE.
              lv_subtraendo = el_vinc_p_flote-qtd_vinc.

            ENDIF.
**** Verifica se o Documento é da ZSDTPROD_FLOTE.
            IF el_vinc_p_flote-vinc_virtual IS INITIAL.

              LOOP AT tl_prod_flote INTO DATA(el_prod_flote) WHERE docnum EQ el_vinc_p_flote-docnum_eprod.
                DATA(lv_index) = sy-tabix.
                IF lv_subtraendo LE el_prod_flote-saldo_disponivel.
* Do Nothing.
                ELSE.
                  lv_subtraendo = el_prod_flote-saldo_disponivel.

                ENDIF.
*** Tratando Notas de Produtor para Vincular (ZSDTPROD_FLOTE).
                READ TABLE tl_prod_flote_up INTO DATA(el_prod_flote_up) WITH KEY docnum = el_prod_flote-docnum
                                                                                 itmnum = el_prod_flote-itmnum
                                                                                 werks  = el_prod_flote-werks
                                                                                 seq    = el_prod_flote-seq
                                                                                 cancel = abap_on.

                IF NOT sy-subrc IS INITIAL.
* Cancela o registro atual.
                  MOVE: abap_on  TO el_prod_flote-cancel,
                        sy-uname TO el_prod_flote-us_cancel,
                        sy-datlo TO el_prod_flote-dt_cancel,
                        sy-timlo TO el_prod_flote-hr_cancel.

                  APPEND el_prod_flote TO tl_prod_flote_up.

                ENDIF.

                el_prod_flote-saldo_vinc       = el_prod_flote-saldo_vinc + lv_subtraendo.
                el_prod_flote-saldo_disponivel = el_prod_flote-saldo_disponivel - lv_subtraendo.
                MODIFY tl_prod_flote FROM el_prod_flote INDEX lv_index TRANSPORTING saldo_vinc saldo_disponivel.
* Insere o novo registro.
                CLEAR: el_prod_flote-cancel, el_prod_flote-us_cancel, el_prod_flote-dt_cancel, el_prod_flote-hr_cancel.
                MOVE: sy-uname TO el_prod_flote-us_criacao,
                      sy-datlo TO el_prod_flote-dt_criacao,
                      sy-timlo TO el_prod_flote-hr_criacao.
* Se saldo "zerado", marcar como saldo não disponível.
                IF el_prod_flote-saldo_disponivel IS INITIAL.
                  el_prod_flote-saldo_nao_disponivel = abap_on.

                ENDIF.

                CLEAR el_prod_flote_up.
                READ TABLE tl_prod_flote_up INTO el_prod_flote_up WITH KEY docnum = el_prod_flote-docnum
                                                                           itmnum = el_prod_flote-itmnum
                                                                           werks  = el_prod_flote-werks
                                                                           cancel = abap_off.

                IF sy-subrc IS INITIAL.
                  el_prod_flote-seq = el_prod_flote_up-seq.
                  MODIFY tl_prod_flote_up FROM el_prod_flote INDEX sy-tabix.

                ELSE.
* Gera o número sequencial do campo SEQ das tabelas PROD_FLOTE, FLOTE_FLOTE, FL_FLOTE_REF.
                  PERFORM zf_number_get_next IN PROGRAM zsdr0012 USING    '01'
                                                                          'ZSEQPROD'
                                                              CHANGING el_prod_flote-seq.

                  APPEND el_prod_flote TO tl_prod_flote_up.

                ENDIF.

                DATA(lv_done) = abap_on.

                vl_quant_vinc = vl_quant_vinc - lv_subtraendo.

                IF vl_quant_vinc IS INITIAL.
                  EXIT.

                ENDIF.

              ENDLOOP.
**** Documento é da ZSDTFLOTE_FLOTE.
            ELSE.
              LOOP AT tl_flote_flote_vit INTO el_flote_flote WHERE docnum EQ el_vinc_p_flote-docnum_eprod.
                lv_index = sy-tabix.
                IF lv_subtraendo LE el_flote_flote-saldo_disponivel.
* Do nothing.
                ELSE.
                  lv_subtraendo = el_flote_flote-saldo_disponivel.

                ENDIF.
*** Tratando Notas de Produtor para Vincular (ZSDTFLOTE_FLOTE).
                READ TABLE tl_flote_flote_up INTO DATA(el_flote_flote_up) WITH KEY docnum = el_flote_flote-docnum
                                                                                   itmnum = el_flote_flote-itmnum
                                                                                   werks  = el_flote_flote-werks
                                                                                   seq    = el_flote_flote-seq
                                                                                   cancel = abap_on.

                IF NOT sy-subrc IS INITIAL.
* Cancela o registro atual.
                  MOVE: abap_on  TO el_flote_flote-cancel,
                        sy-uname TO el_flote_flote-us_cancel,
                        sy-datlo TO el_flote_flote-dt_cancel,
                        sy-timlo TO el_flote_flote-hr_cancel.

                ENDIF.

                APPEND el_flote_flote TO tl_flote_flote_up.

                el_flote_flote-saldo_vinc       = el_flote_flote-saldo_vinc + lv_subtraendo.
                el_flote_flote-saldo_disponivel = el_flote_flote-saldo_disponivel - lv_subtraendo.
                MODIFY tl_flote_flote_vit FROM el_flote_flote INDEX lv_index TRANSPORTING saldo_vinc saldo_disponivel.
* Insere o novo registro.
                CLEAR: el_flote_flote-cancel, el_flote_flote-us_cancel, el_flote_flote-dt_cancel, el_flote_flote-hr_cancel.
                MOVE: sy-uname TO el_flote_flote-us_criacao,
                      sy-datlo TO el_flote_flote-dt_criacao,
                      sy-timlo TO el_flote_flote-hr_criacao.
* Se saldo "zerado", marcar como saldo não disponível.
                IF el_flote_flote-saldo_disponivel IS INITIAL.
                  el_flote_flote-saldo_nao_disponivel = abap_on.

                ENDIF.

                READ TABLE tl_flote_flote_up INTO el_flote_flote_up WITH KEY docnum = el_flote_flote-docnum
                                                                             itmnum = el_flote_flote-itmnum
                                                                             werks  = el_flote_flote-werks
                                                                             seq    = el_flote_flote-seq
                                                                             cancel = abap_off.

                IF sy-subrc IS INITIAL.
                  el_flote_flote-seq = el_flote_flote_up-seq.
                  MODIFY tl_flote_flote_up FROM el_flote_flote INDEX sy-tabix.

                ELSE.
* Gera o número sequencial do campo SEQ das tabelas PROD_FLOTE, FLOTE_FLOTE, FL_FLOTE_REF.
                  PERFORM zf_number_get_next IN PROGRAM zsdr0012 USING    '01'
                                                                          'ZSEQFLOTE'
                                                              CHANGING el_flote_flote-seq.

                  APPEND el_flote_flote TO tl_flote_flote_up.

                ENDIF.

                vl_quant_vinc2       = lv_subtraendo.
                DATA(lv_subtraendo2) = lv_subtraendo.

                LOOP AT tl_fl_flote_ref_vit INTO el_fl_flote_ref WHERE docnum_flote EQ el_flote_flote-docnum.
                  lv_index = sy-tabix.
                  IF lv_subtraendo LE el_fl_flote_ref-saldo_disponivel.
* Do nothing.
                  ELSE.
                    lv_subtraendo2 = el_fl_flote_ref-saldo_disponivel.

                  ENDIF.
*** Tratando Notas de Produtor para Vincular (ZSDTFLOTE_FLOTE).
* Cancela o registro atual.
                  MOVE: abap_on  TO el_fl_flote_ref-cancel,
                        sy-uname TO el_fl_flote_ref-us_cancel,
                        sy-datlo TO el_fl_flote_ref-dt_cancel,
                        sy-timlo TO el_fl_flote_ref-hr_cancel.

                  APPEND el_fl_flote_ref TO tl_fl_flote_ref_up.

                  CLEAR: el_fl_flote_ref-cancel, el_fl_flote_ref-us_cancel, el_fl_flote_ref-dt_cancel, el_fl_flote_ref-hr_cancel.

                  el_fl_flote_ref-qtd_vinc         = el_fl_flote_ref-qtd_vinc + lv_subtraendo2.
                  el_fl_flote_ref-saldo_disponivel = el_fl_flote_ref-saldo_disponivel - lv_subtraendo2.
                  MODIFY tl_fl_flote_ref_vit FROM el_fl_flote_ref INDEX lv_index TRANSPORTING qtd_vinc saldo_disponivel.
* Insere o novo registro.
                  MOVE: sy-uname TO el_fl_flote_ref-us_criacao,
                        sy-datlo TO el_fl_flote_ref-dt_criacao,
                        sy-timlo TO el_fl_flote_ref-hr_criacao.
* Gera o número sequencial do campo SEQ das tabelas PROD_FLOTE, FLOTE_FLOTE, FL_FLOTE_REF.
                  PERFORM zf_number_get_next IN PROGRAM zsdr0012 USING    '01'
                                                                          'ZSEQFLREF'
                                                              CHANGING el_fl_flote_ref-seq.
                  APPEND el_fl_flote_ref TO tl_fl_flote_ref_up.

                  READ TABLE tl_vinc_p_flote_all INTO DATA(el_vinc_p_flote_all) WITH KEY docnum_flote = el_retlote-docnum
                                                                                         docnum_eprod = el_fl_flote_ref-docnum_eprod
                                                                                         docnum_ref   = el_fl_flote_ref-docnum_flote.

                  IF sy-subrc IS INITIAL.

                    IF el_vinc_p_flote_all-cancel IS INITIAL.
* Marca linha como cancelado ZSDTVINC_P_FLOTE.
                      MOVE: abap_on  TO el_vinc_p_flote_all-cancel,
                            sy-uname TO el_vinc_p_flote_all-us_cancel,
                            sy-datlo TO el_vinc_p_flote_all-dt_cancel,
                            sy-timlo TO el_vinc_p_flote_all-hr_cancel.

                      APPEND el_vinc_p_flote_all TO tl_vinc_p_flote_up.
* Cria uma nova linha ZSDTVINC_P_FLOTE.
                      el_vinc_p_flote_all-qtd_vinc = el_vinc_p_flote_all-qtd_vinc + lv_subtraendo2.

                    ELSE.
                      el_vinc_p_flote_all-qtd_vinc = lv_subtraendo2.

                    ENDIF.
* Cria uma nova linha ZSDTVINC_P_FLOTE.
                    CLEAR: el_vinc_p_flote_all-cancel, el_vinc_p_flote_all-us_cancel, el_vinc_p_flote_all-dt_cancel, el_vinc_p_flote_all-hr_cancel.
                    el_vinc_p_flote_all-us_criacao = sy-uname.
                    el_vinc_p_flote_all-dt_criacao = sy-datlo.
                    el_vinc_p_flote_all-hr_criacao = sy-timlo.
* Busca o último valor do campo ID_VINC da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
                    SELECT SINGLE MAX( id_vinc ) FROM zsdtvinc_p_flote
                      INTO el_vinc_p_flote_all-id_vinc
                    WHERE docnum_flote EQ el_vinc_p_flote_all-docnum_flote
                      AND docnum_eprod EQ el_vinc_p_flote_all-docnum_eprod
                      AND docnum_ref   EQ el_vinc_p_flote_all-docnum_ref.

                    IF sy-subrc IS INITIAL.
                      ADD 1 TO el_vinc_p_flote_all-id_vinc.

                    ELSE.
                      el_vinc_p_flote_all-id_vinc = 1.

                    ENDIF.

                    APPEND el_vinc_p_flote_all TO tl_vinc_p_flote_up.

                  ENDIF.

                  lv_done        = abap_on.
                  vl_quant_vinc2 = vl_quant_vinc2 - lv_subtraendo2.

                  IF vl_quant_vinc2 IS INITIAL.
                    EXIT.

                  ENDIF.

                ENDLOOP.

                lv_done       = abap_on.
                vl_quant_vinc = vl_quant_vinc - lv_subtraendo.

                IF vl_quant_vinc IS INITIAL.
                  EXIT.

                ENDIF.

              ENDLOOP.

            ENDIF.
* Verifica se houve processamento
            IF NOT lv_done IS INITIAL.
*** Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
              IF el_vinc_p_flote-cancel IS INITIAL.
                READ TABLE tl_vinc_p_flote_all INTO el_vinc_p_flote WITH KEY docnum_flote = el_vinc_p_flote-docnum_flote
                                                                             docnum_eprod = el_vinc_p_flote-docnum_eprod
                                                                             id_vinc      = el_vinc_p_flote-id_vinc
                                                                             docnum_ref   = el_vinc_p_flote-docnum_ref.

                IF sy-subrc IS INITIAL.
                  MOVE: abap_on  TO el_vinc_p_flote-cancel,
                        sy-uname TO el_vinc_p_flote-us_cancel,
                        sy-datlo TO el_vinc_p_flote-dt_cancel,
                        sy-timlo TO el_vinc_p_flote-hr_cancel.

                  APPEND el_vinc_p_flote TO tl_vinc_p_flote_up.

                ENDIF.

                el_vinc_p_flote-qtd_vinc = el_vinc_p_flote-qtd_vinc + lv_subtraendo.

              ELSE.
                el_vinc_p_flote-qtd_vinc = lv_subtraendo.

              ENDIF.

              CLEAR: el_vinc_p_flote-cancel, el_vinc_p_flote-us_cancel, el_vinc_p_flote-dt_cancel, el_vinc_p_flote-hr_cancel.
              MOVE: sy-uname TO el_vinc_p_flote-us_criacao,
                    sy-datlo TO el_vinc_p_flote-dt_criacao,
                    sy-timlo TO el_vinc_p_flote-hr_criacao.
* Busca o último valor do campo ID_VINC da Tabela de vinculo entre a formação de lote (ZSDTVINC_P_FLOTE).
              SELECT SINGLE MAX( id_vinc ) FROM zsdtvinc_p_flote
                INTO el_vinc_p_flote-id_vinc
              WHERE docnum_flote EQ el_vinc_p_flote-docnum_flote
                AND docnum_eprod EQ el_vinc_p_flote-docnum_eprod
                AND docnum_ref   EQ el_vinc_p_flote-docnum_ref.

              IF sy-subrc IS INITIAL.
                ADD 1 TO el_vinc_p_flote-id_vinc.

              ELSE.
                el_vinc_p_flote-id_vinc = 1.

              ENDIF.

              APPEND el_vinc_p_flote TO tl_vinc_p_flote_up.
              CLEAR lv_done.

            ENDIF.

            IF vl_quant_vinc IS INITIAL.
              EXIT.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDLOOP.

    ENDLOOP.
*** Atualização das tabelas do processamento.
* Tabela de Notas de formação de lote Recusada para Vincular.
    IF NOT tl_flote_flote_up[] IS INITIAL.
      MODIFY zsdtflote_flote FROM TABLE tl_flote_flote_up.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.

      ELSE.
        ROLLBACK WORK.

      ENDIF.

    ENDIF.
* Tabela de Tabela auxiliar - Controle do Saldo do Liberado pelo Retorno.
    IF NOT tl_fl_flote_ref_up[] IS INITIAL.
      MODIFY zsdtfl_flote_ref FROM TABLE tl_fl_flote_ref_up.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.

      ELSE.
        ROLLBACK WORK.

      ENDIF.

    ENDIF.
* Tabela de Tabela de vinculo entre a formação de lote.
    IF NOT tl_vinc_p_flote_up[] IS INITIAL.
      MODIFY zsdtvinc_p_flote FROM TABLE tl_vinc_p_flote_up.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.

      ELSE.
        ROLLBACK WORK.

      ENDIF.

    ENDIF.
* Tabela de Notas de Produtor para Vincular.
    IF NOT tl_prod_flote_up[] IS INITIAL.
      MODIFY zsdtprod_flote FROM TABLE tl_prod_flote_up.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.

      ELSE.
        ROLLBACK WORK.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_controle_retorno_rfl~validar_cancelamento_retorno.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZIF_CONTROLE_RETORNO_RFL~VALIDAR_CANCELAMENTO_RETORNO                                *
*& Chamado        : USER STORY 157683                                                                    *
*& Data           : 11/11/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 11/11/2024|DEVK9A1XAW |NSEGATIN       | Verificação de impedimento de cancelamento do retorno         *
*--------------------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Types                                                                *
*----------------------------------------------------------------------*
    TYPES: BEGIN OF ty_nf_entradas,
             docnum TYPE j_1bnflin-docnum,
             itmnum TYPE j_1bnflin-itmnum,
             matnr  TYPE j_1bnflin-matnr,
             cfop   TYPE j_1bnflin-cfop,
             menge  TYPE j_1bnflin-menge,
             werks  TYPE j_1bnflin-werks,
             matkl  TYPE j_1bnflin-matkl,
             docdat TYPE j_1bnfdoc-docdat,
             cancel TYPE j_1bnfdoc-cancel,
           END OF ty_nf_entradas.
*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
    DATA: tl_export           TYPE TABLE OF zsdt_export,
          tl_notas_retorno    TYPE TABLE OF ty_nf_entradas,
          tl_retlote          TYPE TABLE OF zsdt_retlote,
          tl_vinc_p_flote_aux TYPE          zsdtvinc_p_flote_t.
*----------------------------------------------------------------------*
* Ranges                                                               *
*----------------------------------------------------------------------*
    DATA: rl_cfop_retorno TYPE RANGE OF j_1bcfop,
          rl_matkl        TYPE RANGE OF matkl,
          rl_final_ret_r  TYPE RANGE OF zfin_export.
*----------------------------------------------------------------------*
* Variáveis                                                            *
*----------------------------------------------------------------------*
    DATA: vl_data_val TYPE sy-datum,
          vl_sld_disp TYPE zsaldo_disponivel,
          vl_qtd_vinc TYPE j_1bnetqty.
*----------------------------------------------------------------------*
* Constantes                                                            *
*----------------------------------------------------------------------*
    CONSTANTS: cl_ieq TYPE char3 VALUE 'IEQ'.

*** CFOP Retorno de formação de lote
    SELECT * FROM zsdt0355
      INTO TABLE @DATA(lt_zsdt0355)
      WHERE excluido EQ @space.

    IF sy-subrc IS INITIAL.
      rl_cfop_retorno = VALUE #( FOR el_zsdt0355 IN lt_zsdt0355 ( sign = cl_ieq(1) option = cl_ieq+1(2) low = el_zsdt0355-cfop ) ).

    ENDIF.
*** Grupo de materiais
    SELECT *
      FROM zsdt0356
      INTO TABLE @DATA(lt_zsdt0356)
      WHERE excluido EQ @space.

    IF sy-subrc IS INITIAL.
      rl_matkl = VALUE #( FOR el_zsdt0356 IN lt_zsdt0356 ( sign = cl_ieq(1) option = cl_ieq+1(2) low = el_zsdt0356-matkl ) ).

    ENDIF.
*** Finalidades RFL - Fila NF Retorno de Recusa.
    SELECT * FROM zsdt0364 INTO TABLE @DATA(tl_zsdt0364) WHERE excluido EQ @space.

    IF sy-subrc IS INITIAL.
      rl_final_ret_r = VALUE #( FOR el_zsdt0364 IN tl_zsdt0364 ( sign = 'I' option = 'EQ' low = el_zsdt0364-finalidade ) ).

    ENDIF.

    SELECT li~docnum li~itmnum li~matnr li~cfop li~menge li~werks li~matkl dc~docdat dc~cancel
     FROM j_1bnfdoc AS dc
     INNER JOIN j_1bnflin AS li
      ON li~docnum EQ dc~docnum
     INTO TABLE tl_notas_retorno
    WHERE dc~docnum EQ i_docnum
      AND dc~direct EQ '1'
      AND dc~model  EQ '55'
      AND dc~nftype EQ 'ZV'
      AND dc~doctyp EQ '6'
      AND dc~cancel EQ i_doc_cancel
      AND li~cfop   IN rl_cfop_retorno
      AND li~matkl  IN rl_matkl.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(el_doc_retorno) WHERE docnum EQ @i_docnum.

      IF ( sy-subrc IS NOT INITIAL ) OR ( i_docnum IS INITIAL ).
        e_erro = abap_on.
* Documento &1 não encontrado!
        RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
          EXPORTING
            textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgid
                              msgno = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgno
                              attr1 = CONV #( i_docnum )
                             )
            msgty  = sy-abcde+4(1) "E - Erro
            msgno  = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgno
            msgid  = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgid
            msgv1  = CONV #( i_docnum ).

      ENDIF.

      SELECT SINGLE *
        FROM zfiwrt0008 INTO @DATA(el_zfiwrt008)
       WHERE docret_quebra   EQ @i_docnum
         AND docs_estornados EQ @abap_false
         AND loekz           EQ @abap_false.

      IF sy-subrc IS INITIAL.
        e_erro = abap_on.
* Doc. ZNFW &1 já gerado para o Documento Retorno &2!
        RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
          EXPORTING
            textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_exists_retorno_quebra-msgid
                              msgno = zcx_controle_retorno_rfl=>zcx_exists_retorno_quebra-msgno
                              attr1 = CONV #( el_zfiwrt008-seq_lcto )
                              attr2 = CONV #( i_docnum )
                             )
            msgty  = sy-abcde+4(1) "E - Erro
            msgno  = zcx_controle_retorno_rfl=>zcx_exists_retorno_quebra-msgno
            msgid  = zcx_controle_retorno_rfl=>zcx_exists_retorno_quebra-msgid
            msgv1  = CONV #( el_zfiwrt008-seq_lcto )
            msgv2  = CONV #( i_docnum ).

      ENDIF.

      CALL FUNCTION 'Z_RET_DATA_MES_ABERTO'
        EXPORTING
          p_data_ent  = el_doc_retorno-pstdat
          p_bukrs     = el_doc_retorno-bukrs
        IMPORTING
          p_data_val  = vl_data_val
        EXCEPTIONS
          sem_periodo = 1
          OTHERS      = 2.

      IF vl_data_val+0(6) NE el_doc_retorno-pstdat+0(6) AND el_doc_retorno-pstdat+4(2) LE 12.
        e_erro = abap_on.
* Periodo FI não esta aberto!
        RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
          EXPORTING
            textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_periodo_fi_fechado-msgid
                              msgno = zcx_controle_retorno_rfl=>zcx_periodo_fi_fechado-msgno )
            msgty  = sy-abcde+4(1) "E - Erro
            msgno  = zcx_controle_retorno_rfl=>zcx_periodo_fi_fechado-msgno
            msgid  = zcx_controle_retorno_rfl=>zcx_periodo_fi_fechado-msgid.

      ENDIF.

      SELECT * FROM zsdt_export INTO TABLE tl_export WHERE docnum EQ i_docnum.

      LOOP AT tl_export INTO DATA(el_export) WHERE ordem  IS NOT INITIAL
                                                OR export IS NOT INITIAL.
        e_erro = abap_on.
* Documento retorno &1 possui exportação vinculada!
        RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
          EXPORTING
            textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_doc_ret_exists_exp-msgid
                              msgno = zcx_controle_retorno_rfl=>zcx_doc_ret_exists_exp-msgno
                              attr1 = CONV #( i_docnum )
                            )
            msgty  = sy-abcde+4(1) "E - Erro
            msgno  = zcx_controle_retorno_rfl=>zcx_doc_ret_exists_exp-msgno
            msgid  = zcx_controle_retorno_rfl=>zcx_doc_ret_exists_exp-msgid
            msgv1  = CONV #( i_docnum ).

      ENDLOOP.

      READ TABLE tl_export INTO el_export INDEX 1.

      IF     sy-subrc              IS INITIAL AND
         NOT el_export-process_1x1 IS INITIAL.

        SELECT * FROM zsdt_retlote INTO TABLE tl_retlote WHERE docnum_ret EQ el_export-docnum.

        IF ( sy-subrc IS NOT INITIAL ) OR ( i_docnum IS INITIAL ).
          e_erro = abap_on.
* Documento retorno &1 não encontrado!
          RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
            EXPORTING
              textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_documento_ret_not_found-msgid
                                msgno = zcx_controle_retorno_rfl=>zcx_documento_ret_not_found-msgno
                                attr1 = CONV #( i_docnum )
                               )
              msgty  = sy-abcde+4(1) "E - Erro
              msgno  = zcx_controle_retorno_rfl=>zcx_documento_ret_not_found-msgno
              msgid  = zcx_controle_retorno_rfl=>zcx_documento_ret_not_found-msgid
              msgv1  = CONV #( i_docnum ).

        ENDIF.

        DATA(tl_retlote_aux) = tl_retlote.
        SORT tl_retlote_aux BY docnum.
        DELETE ADJACENT DUPLICATES FROM tl_retlote_aux COMPARING docnum.

        DATA tl_vinc_p_flote TYPE zsdtvinc_p_flote_t.

        SELECT * FROM zsdtvinc_p_flote
          INTO TABLE tl_vinc_p_flote
          FOR ALL ENTRIES IN tl_retlote_aux
        WHERE docnum_flote EQ tl_retlote_aux-docnum.

        IF NOT tl_vinc_p_flote IS INITIAL.
* Tbl. lógica - Vinculo formação de lote para Reverter Retorno
          zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->vinc_flote_reverte_retorno( EXPORTING it_vinc_p_flote_in  = tl_vinc_p_flote
                                                                                                          IMPORTING et_vinc_p_flote_out = tl_vinc_p_flote ).

        ENDIF.

        SELECT docnum, itmnum, ownpro
          FROM j_1bnflin
          INTO TABLE @DATA(lt_retorno)
          FOR ALL ENTRIES IN @tl_retlote_aux
        WHERE docnum EQ @tl_retlote_aux-docnum.

        LOOP AT tl_retlote INTO DATA(el_retlote).
          ADD  el_retlote-quant_vinc TO vl_qtd_vinc.

        ENDLOOP.

        e_erro = abap_on.
        LOOP AT tl_retlote INTO el_retlote.
* Verifica o tipo da Finalidade de Exportação.
          IF el_export-finalidade IN rl_final_ret_r. "Fila NF Retorno de Recusa

            LOOP AT tl_vinc_p_flote INTO DATA(el_vinc_p_flote) WHERE docnum_flote EQ el_retlote-docnum
                                                                 AND docnum_ref   IS INITIAL.
* Verifica se é virtual ou não.
              CASE el_vinc_p_flote-vinc_virtual.
                WHEN abap_off. "Registro PROD_FLOTE
                  SELECT docnum, saldo_disponivel
                    FROM zsdtprod_flote
                    INTO TABLE @DATA(tl_prod_flote)
                  WHERE docnum EQ @el_vinc_p_flote-docnum_eprod
                    AND cancel EQ @abap_off.

                  IF sy-subrc IS INITIAL.
                    LOOP AT tl_prod_flote INTO DATA(el_prod_flote).
                      ADD el_prod_flote-saldo_disponivel TO vl_sld_disp.

                      IF vl_sld_disp GE vl_qtd_vinc.
                        EXIT.

                      ENDIF.

                    ENDLOOP.

                  ENDIF.

                WHEN abap_on.  "Registro FLOTE_FLOTE
                  SELECT docnum, saldo_disponivel
                    FROM zsdtflote_flote
                    INTO TABLE @DATA(tl_flote_flote)
                  WHERE docnum EQ @el_vinc_p_flote-docnum_eprod
                    AND cancel EQ @abap_off.

                  IF sy-subrc IS INITIAL.
                    LOOP AT tl_flote_flote INTO DATA(el_lote_flote).
                      ADD el_lote_flote-saldo_disponivel TO vl_sld_disp.

                      IF vl_sld_disp GE vl_qtd_vinc.
                        EXIT.

                      ENDIF.

                    ENDLOOP.

                  ENDIF.

                WHEN OTHERS.
*           Do nothing
              ENDCASE.

            ENDLOOP.

          ELSE.
            DATA(el_lote_flote_bk) = el_lote_flote.
            CLEAR: el_lote_flote_bk.
* Verifica se a Nota de Retorno é de Produção Própria.
            READ TABLE lt_retorno INTO DATA(el_retorno) WITH KEY docnum = el_retlote-docnum
                                                                 ownpro = abap_on.

            IF sy-subrc IS INITIAL.
              sy-subrc = 4.
* Verifica se há registro na Tabela de Vínculo para processar normalmente
              LOOP AT tl_vinc_p_flote TRANSPORTING NO FIELDS WHERE docnum_flote EQ el_retlote-docnum
                                                               AND docnum_ref   IS INITIAL.
                CLEAR sy-subrc.
                EXIT.

              ENDLOOP.

              IF NOT sy-subrc IS INITIAL.
                SELECT docnum saldo_disponivel
                  FROM zsdtflote_flote
                  INTO TABLE tl_flote_flote
                WHERE docnum EQ el_retorno-docnum
                  AND cancel EQ abap_off.

                IF sy-subrc IS INITIAL.
                  LOOP AT tl_flote_flote INTO el_lote_flote.
                    ADD el_lote_flote-saldo_disponivel TO vl_sld_disp.

                    IF vl_sld_disp GE vl_qtd_vinc.
                      EXIT.

                    ENDIF.

                  ENDLOOP.

                ENDIF.

              ENDIF.

            ENDIF.

            LOOP AT tl_vinc_p_flote INTO el_vinc_p_flote WHERE docnum_flote EQ el_retlote-docnum
                                                           AND docnum_ref   IS INITIAL.
* Verifica se não é virtual.
              IF el_vinc_p_flote-vinc_virtual IS INITIAL.
                SELECT docnum saldo_disponivel
                  FROM zsdtflote_flote
                  INTO TABLE tl_flote_flote
                WHERE docnum EQ el_vinc_p_flote-docnum_flote
                  AND cancel EQ abap_off.

                IF NOT el_retorno-ownpro IS INITIAL.
                  SELECT docnum saldo_disponivel
                    FROM zsdtflote_flote
                    APPENDING TABLE tl_flote_flote
                  WHERE docnum EQ el_vinc_p_flote-docnum_eprod
                    AND cancel EQ abap_off.

                ENDIF.

              ELSE.
                SELECT docnum saldo_disponivel
                  FROM zsdtflote_flote
                  INTO TABLE tl_flote_flote
                WHERE docnum EQ el_vinc_p_flote-docnum_eprod
                  AND cancel EQ abap_off.

                IF NOT el_retorno-ownpro IS INITIAL.
                  SELECT docnum saldo_disponivel
                    FROM zsdtflote_flote
                    APPENDING TABLE tl_flote_flote
                  WHERE docnum EQ el_vinc_p_flote-docnum_flote
                    AND cancel EQ abap_off.

                ENDIF.

              ENDIF.

              LOOP AT tl_flote_flote INTO el_lote_flote.

                IF el_lote_flote_bk IS INITIAL.
                  el_lote_flote_bk = el_lote_flote.

                ELSE.
                  IF el_lote_flote_bk-docnum NE el_lote_flote-docnum.
                    el_lote_flote_bk = el_lote_flote.

                  ENDIF.

                ENDIF.

                IF el_vinc_p_flote-qtd_vinc LE el_lote_flote_bk-saldo_disponivel.
                  ADD el_vinc_p_flote-qtd_vinc TO vl_sld_disp.
                  SUBTRACT el_vinc_p_flote-qtd_vinc FROM el_lote_flote_bk-saldo_disponivel.

                ELSE.
                  ADD el_lote_flote_bk-saldo_disponivel TO vl_sld_disp.
                  SUBTRACT el_lote_flote-saldo_disponivel FROM el_lote_flote_bk-saldo_disponivel.

                ENDIF.

                IF vl_sld_disp GE vl_qtd_vinc.
                  EXIT.

                ENDIF.

              ENDLOOP.

              IF vl_sld_disp GE vl_qtd_vinc.
                EXIT.

              ENDIF.

            ENDLOOP.

          ENDIF.

          IF vl_sld_disp GE vl_qtd_vinc.
            CLEAR e_erro.
            EXIT.

          ENDIF.

        ENDLOOP.

      ELSE.
        e_erro = abap_on.

        IF NOT sy-subrc IS INITIAL.
          RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
            EXPORTING
              textid = VALUE #( msgid = '00'
                                msgno = '398'
                                attr1 = 'Doc não encontrado na tabela Quantidade Exportada.'
                                attr2 = 'Nº Documento'
                                attr3 = CONV #( i_docnum )
                               )
              msgty  = sy-abcde+4(1) "E - Erro
              msgno  = '00'
              msgid  = '398'
              msgv1  = 'Doc não encontrado na tabela Quantidade Exportada.'
              msgv2  = 'Nº Documento'
              msgv3  = CONV #( i_docnum ).

        ENDIF.

        IF el_export-process_1x1 IS INITIAL.
          RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
            EXPORTING
              textid = VALUE #( msgid = '00'
                                msgno = '398'
                                attr1 = 'Doc. não processada pelo programa 1x1.'
                                attr2 = 'Nº Documento'
                                attr3 = CONV #( i_docnum )
                               )
              msgty  = sy-abcde+4(1) "E - Erro
              msgno  = '00'
              msgid  = '398'
              msgv1  = 'Doc. não processada pelo programa 1x1.'
              msgv2  = 'Nº Documento'
              msgv3  = CONV #( i_docnum ).

        ENDIF.

      ENDIF.
* Verifica se houve erro na validação.
      CHECK NOT e_erro IS INITIAL.
      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = 'ZSD'
                            msgno = '001'
                            attr1 = CONV #( TEXT-e01 ) "NF de F.L. vinculadas ao retorno já estão
                            attr2 = CONV #( TEXT-e02 ) "vinculadas em outras notas de formação de lote.
                            attr3 = CONV #( TEXT-e03 ) "Entrar em contato com o time do custos/estoque!
                            attr4 = CONV #( i_docnum )
                          )
          msgty  = sy-abcde+4(1) "E - Erro
          msgno  = '001'
          msgid  = 'ZSD'
          msgv1  = CONV #( TEXT-e01 )  "NF de F.L. vinculadas ao retorno já estão
          msgv2  = CONV #( TEXT-e02 )  "vinculadas em outras notas de formação de lote.
          msgv3  = CONV #( TEXT-e03 )  "Entrar em contato com o time do custos/estoque!
          msgv4  = CONV #( i_docnum ).

    ELSE.
      e_erro = abap_on.
* Documento &1 não encontrado!
      RAISE EXCEPTION TYPE zcx_controle_retorno_rfl
        EXPORTING
          textid = VALUE #( msgid = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgid
                            msgno = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgno
                            attr1 = CONV #( i_docnum )
                           )
          msgty  = sy-abcde+4(1) "E - Erro
          msgno  = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgno
          msgid  = zcx_controle_retorno_rfl=>zcx_documento_not_found-msgid
          msgv1  = CONV #( i_docnum ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_controle_retorno_rfl~vinc_flote_reverte_retorno.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZIF_CONTROLE_RETORNO_RFL~VINC_FLOTE_REVERTE_RETORNO                                  *
*& Chamado        : USER STORY 157683                                                                    *
*& Data           : 27/11/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 27/11/2024|DEVK9A1XAW |NSEGATIN       | Tbl. lógica - Vinculo formação de lote para Reverter Retorno  *
*--------------------------------------------------------------------------------------------------------*

    DATA(tl_vinc_p_flote2) = it_vinc_p_flote_in.
    DATA(tl_vinc_p_flote3) = it_vinc_p_flote_in.
    CLEAR: it_vinc_p_flote_in, et_vinc_p_flote_out.

    SORT: tl_vinc_p_flote2 BY docnum_flote docnum_eprod docnum_ref id_vinc,
          tl_vinc_p_flote3 BY docnum_flote docnum_eprod docnum_ref ASCENDING id_vinc DESCENDING.

    LOOP AT tl_vinc_p_flote2 INTO DATA(el_vinc_p_flote).
      READ TABLE tl_vinc_p_flote3 INTO DATA(el_vinc_p_flote3) WITH KEY docnum_flote = el_vinc_p_flote-docnum_flote
                                                                       docnum_eprod = el_vinc_p_flote-docnum_eprod
                                                                       docnum_ref   = el_vinc_p_flote-docnum_ref.

      IF sy-subrc IS INITIAL.
        IF el_vinc_p_flote-id_vinc EQ el_vinc_p_flote3-id_vinc.
          APPEND el_vinc_p_flote TO et_vinc_p_flote_out.

        ELSE.
          IF el_vinc_p_flote3-cancel IS INITIAL.
            el_vinc_p_flote-qtd_vinc  = el_vinc_p_flote-qtd_vinc - el_vinc_p_flote3-qtd_vinc.
            el_vinc_p_flote-id_vinc   = el_vinc_p_flote3-id_vinc.
            el_vinc_p_flote-cancel    = el_vinc_p_flote3-cancel.
            el_vinc_p_flote-us_cancel = el_vinc_p_flote3-us_cancel.
            el_vinc_p_flote-dt_cancel = el_vinc_p_flote3-dt_cancel.
            el_vinc_p_flote-hr_cancel = el_vinc_p_flote3-hr_cancel.

          ENDIF.

          IF el_vinc_p_flote-qtd_vinc NE 0.
            APPEND el_vinc_p_flote TO et_vinc_p_flote_out.

          ENDIF.

          DELETE tl_vinc_p_flote2 WHERE docnum_flote EQ el_vinc_p_flote3-docnum_flote
                                    AND docnum_eprod EQ el_vinc_p_flote3-docnum_eprod
                                    AND docnum_ref   EQ el_vinc_p_flote3-docnum_ref.

        ENDIF.

      ENDIF.

    ENDLOOP.

    SORT et_vinc_p_flote_out BY docnum_flote docnum_eprod id_vinc docnum_ref.

  ENDMETHOD.
ENDCLASS.
