*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: RODRIGO C.                                              &*
*& Data.....: 09/10/2023                                              &*
*& Descrição: JOB integrando requisição criada através das ordens PM  &*
*&            com o sistema MRP Coupa.                                &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zmmr191.


*----------------------------------------------------------------------*
* Tabelas                                                          *
*----------------------------------------------------------------------*
TABLES: zmms_dados_int_coupa_alv.


*----------------------------------------------------------------------*
* Tipo de Tabelas                                                          *
*----------------------------------------------------------------------*
*TYPES:
*
*  BEGIN OF ty_dados_integracao,
*  END   OF ty_dados_integracao.


*----------------------------------------------------------------------*
* Tabela interna                                                       *
*----------------------------------------------------------------------*
DATA: gt_eban      TYPE TABLE OF eban,
      gt_envio_om  TYPE TABLE OF zintegrcoupa01,
      gt_aufk      TYPE TABLE OF aufk,
      gt_cskt      TYPE TABLE OF cskt,
      gt_skat      TYPE TABLE OF skat,
      gt_ebkn      TYPE TABLE OF ebkn,
      gt_npact     TYPE TABLE OF v_npact,
      gt_t001      TYPE TABLE OF t001,
      gt_t001k     TYPE TABLE OF t001k,
      gt_esll      TYPE TABLE OF esll,
      gt_esll_sub  TYPE TABLE OF esll,
      gt_t001w     TYPE TABLE OF t001w,
      gt_set_catc  TYPE TABLE OF rgsbv,
      gt_bapiret2  TYPE TABLE OF bapiret2,
      gt_dados_alv TYPE STANDARD TABLE OF zmms_dados_int_coupa_alv,
      gt_dados_aux TYPE STANDARD TABLE OF zmms_dados_int_coupa_alv,
      gt_set       TYPE TABLE OF rgsbv.

DATA go_int TYPE REF TO zcl_integracao_coupa_req_comp.

CONSTANTS gc_service TYPE /ui2/service_name VALUE 'COUPA_INT_ENVIA_REQ_COMPRA'.
CONSTANTS gc_enviado TYPE zemm_status_coupa VALUE 'S'.
CONSTANTS gc_liberado TYPE zemm_status_coupa VALUE 'L'.


*----------------------------------------------------------------------*
* Work Área                                                            *
*----------------------------------------------------------------------*
*DATA: wa_zmmt0172      TYPE zmmt0172.


RANGES: rg_badat FOR zmms_dados_int_coupa_alv-badat.


*----------------------------------------------------------------------*
* Tela de seleção                                                             *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS so_banfn FOR zmms_dados_int_coupa_alv-banfn.
  SELECT-OPTIONS so_bnfpo FOR zmms_dados_int_coupa_alv-bnfpo.

SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
* INITIALIZATION                                                             *
*----------------------------------------------------------------------*
INITIALIZATION.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                             *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM fm_seleciona_req_compras.

  IF NOT gt_eban[] IS INITIAL.

    PERFORM fm_envia_req_compras_mrp_coupa.

    PERFORM fm_enviar_re_compras_mrp_coupa.

  ELSE.
    MESSAGE s016(ds) WITH 'Registros não encontrados' sy-cprog DISPLAY LIKE 'E'.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  fm_seleciona_req_compras
*&---------------------------------------------------------------------*
FORM fm_seleciona_req_compras.

  DATA lr_knttp TYPE RANGE OF knttp.

  PERFORM f_carrega_set USING '0000MAGGI_COUPA_REQ_CATC' CHANGING gt_set_catc.

  LOOP AT gt_set_catc ASSIGNING FIELD-SYMBOL(<fs_catc>).
    APPEND 'IEQ' && <fs_catc>-from TO lr_knttp.
  ENDLOOP.



  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      days      = '30'
      months    = '00'
      signum    = '-'
      years     = '00'
    IMPORTING
      calc_date = rg_badat-low.

  rg_badat-sign   = 'I'.
  rg_badat-option = 'BT'.
  rg_badat-high   = sy-datum.
  APPEND rg_badat.



  " Seleciona Dados de Requisição de compra
  SELECT *
         FROM eban
         INTO TABLE gt_eban
         WHERE banfn      IN so_banfn
         AND bnfpo        IN so_bnfpo
         AND badat        IN rg_badat
         AND knttp        IN lr_knttp
         AND id_coupa     EQ space
*** Stefanini - IR231324 - 04/04/2025 - LAZAROSR - Início de Alteração
*         and frgkz        eq '2'
*         and frgzu        eq 'X'
*** Stefanini - IR231324 - 04/04/2025 - LAZAROSR - Fim de Alteração
         AND ebeln        EQ space
         AND loekz        EQ space.

  CHECK gt_eban[] IS NOT INITIAL.

  CHECK sy-subrc EQ 0.

  SELECT * FROM t001k
    INTO TABLE gt_t001k
      FOR ALL ENTRIES IN gt_eban
        WHERE bwkey	=	gt_eban-werks.

  CHECK sy-subrc EQ 0.

  SELECT * FROM t001
    INTO TABLE gt_t001
      FOR ALL ENTRIES IN gt_t001k
        WHERE bukrs = gt_t001k-bukrs.

  SELECT * FROM t001w
     INTO TABLE gt_t001w
        FOR ALL ENTRIES IN gt_eban
          WHERE werks	=	gt_eban-werks.

  CHECK sy-subrc EQ 0 .

  SELECT * FROM esll
    INTO TABLE gt_esll
      FOR ALL ENTRIES IN gt_eban
        WHERE packno = gt_eban-packno.

  IF sy-subrc EQ 0.

    SELECT * FROM esll
    INTO TABLE gt_esll_sub
      FOR ALL ENTRIES IN gt_esll
        WHERE packno = gt_esll-sub_packno.

  ENDIF.


  SELECT * FROM ebkn
    INTO TABLE gt_ebkn
         FOR ALL ENTRIES IN gt_eban
          WHERE banfn  = gt_eban-banfn
            AND bnfpo = gt_eban-bnfpo.

  IF gt_ebkn IS NOT INITIAL.

    SELECT * FROM v_npact
      INTO TABLE gt_npact
        FOR ALL ENTRIES IN gt_ebkn
          WHERE aufnr  = gt_ebkn-aufnr.

    " 05.08.2022 - RAMON - ENVIO DA ORDEM DE PRODUÇÃO -->
    IF sy-subrc EQ 0.

      DATA lr_id_om TYPE RANGE OF zcoupa_id_integr.
      DATA lv_om TYPE c LENGTH 20.

      LOOP AT gt_npact ASSIGNING FIELD-SYMBOL(<fs_npact>).

        lv_om = <fs_npact>-aufnr.

        SHIFT lv_om LEFT DELETING LEADING '0'.

        APPEND 'IEQ' && lv_om TO lr_id_om.

      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM lr_id_om.

      IF lr_id_om IS NOT INITIAL.

        SELECT * FROM zintegrcoupa01
          INTO TABLE gt_envio_om
            WHERE id_integr IN lr_id_om
              AND ident_proc = 'OM'
              AND status = 'S'.

      ENDIF.

    ENDIF.
    " 05.08.2022 - RAMON - ENVIO DA ORDEM DE PRODUÇÃO --<

    SELECT * FROM skat
      INTO TABLE gt_skat
        FOR ALL ENTRIES IN gt_ebkn
          WHERE saknr = gt_ebkn-sakto
            AND spras = sy-langu
            AND ktopl = '0050'.

    IF sy-subrc EQ 0.

      SELECT * FROM cskt
        INTO TABLE gt_cskt
        FOR ALL ENTRIES IN gt_ebkn
          WHERE spras = sy-langu
           AND kokrs = 'MAGI'
           AND kostl = gt_ebkn-kostl.
    ENDIF.

    SELECT * FROM aufk
      INTO TABLE gt_aufk
      FOR ALL ENTRIES IN gt_ebkn
        WHERE aufnr  = gt_ebkn-aufnr
          AND autyp = '30'.

  ENDIF.


  CALL FUNCTION 'G_SET_FETCH'
    EXPORTING
      setnr           = '0000MAGGI_EMPRESAS_COUPA'
    TABLES
      set_lines_basic = gt_set
    EXCEPTIONS
      no_authority    = 1
      set_is_broken   = 2
      set_not_found   = 3
      OTHERS          = 4.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_SET
*&---------------------------------------------------------------------*
FORM f_carrega_set  USING p_setnr TYPE c
                    CHANGING p_set_tab TYPE rgsbv_tab.

  CALL FUNCTION 'G_SET_FETCH'
    EXPORTING
      setnr           = p_setnr
    TABLES
      set_lines_basic = p_set_tab
    EXCEPTIONS
      no_authority    = 1
      set_is_broken   = 2
      set_not_found   = 3
      OTHERS          = 4.

  IF sy-subrc <> 0.
    "p_erro = 'X'.
    "EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ENVIA_REQ_COMPRAS_MRP_COUPA
*&---------------------------------------------------------------------*
FORM fm_envia_req_compras_mrp_coupa .

  PERFORM f_sap_indicator USING 'processando dados...' 50.

  LOOP AT gt_eban ASSIGNING FIELD-SYMBOL(<fs_eban>).

    READ TABLE gt_t001k ASSIGNING FIELD-SYMBOL(<fs_t001k>) WITH KEY bwkey = <fs_eban>-werks.

    IF sy-subrc NE 0.

      " lançar erro  - "Centro <fs_eban>-werks não existe na T001K"
      PERFORM f_mensagem_insere TABLES gt_bapiret2
                                USING 'E' 'DS' '016' 'Centro' <fs_eban>-werks 'não existe na' 'T001K'.

      IF sy-sysid NE 'DEV'.
        CONTINUE.
      ENDIF.


    ENDIF.

    READ TABLE gt_set ASSIGNING FIELD-SYMBOL(<fs_set>) WITH KEY from = <fs_t001k>-bukrs.

    IF sy-subrc NE 0.

      " lançar erro - "Centro X não está habilitado no COUPA"
      PERFORM f_mensagem_insere TABLES gt_bapiret2
                                USING 'E' 'DS' '016' 'Centro' <fs_eban>-werks 'não está habilitado no' 'COUPA'.

      IF sy-sysid NE 'DEV'.
        CONTINUE.
      ENDIF.

    ENDIF.

    READ TABLE gt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>) WITH KEY werks = <fs_eban>-werks.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_ebkn ASSIGNING FIELD-SYMBOL(<fs_ebkn>) WITH KEY banfn = <fs_eban>-banfn
                                                                  bnfpo = <fs_eban>-bnfpo.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_aufk ASSIGNING FIELD-SYMBOL(<fs_aufk>) WITH KEY aufnr  = <fs_ebkn>-aufnr.


    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados_alv>).

    MOVE-CORRESPONDING <fs_eban> TO <fs_dados_alv>.

    <fs_dados_alv>-bukrs = <fs_t001k>-bukrs.
    <fs_dados_alv>-name1 = <fs_t001w>-name1.
    IF <fs_ebkn> IS ASSIGNED.
      <fs_dados_alv>-aufnr = <fs_ebkn>-aufnr.
    ENDIF.

    ADD 1 TO <fs_dados_alv>-total_itens.

    LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_check>) WHERE banfn = <fs_dados_alv>-banfn AND bnfpo NE <fs_dados_alv>-bnfpo.
      ADD 1 TO <fs_dados_alv>-total_itens.
    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere  TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator  USING p_text    TYPE c
                            p_percent TYPE i.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percent
      text       = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ENVIAR_RE_COMPRAS_MRP_COUPA
*&---------------------------------------------------------------------*
FORM fm_enviar_re_compras_mrp_coupa .

  DATA lv_status_coupa TYPE zemm_status_coupa.
  DATA lv_id_coupa TYPE zemm_id_coupa.

  DATA lv_count TYPE i.
  DATA lv_erro TYPE c.
  DATA lt_dados_envio TYPE TABLE OF zmms_dados_int_coupa_eban.
  DATA lt_inter_tab TYPE zmmc_dados_int_coupa_intermedi.

  DATA lt_dados_retorno TYPE zmmc_dados_int_coupa_ret.


  SORT gt_dados_alv BY banfn bnfpo.


  CLEAR gt_bapiret2.
  REFRESH gt_dados_aux. "*BUG SOLTO 174842
  LOOP AT gt_dados_alv INTO DATA(lw_dados).

    READ TABLE gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) INDEX sy-tabix.
    IF <fs_dados>-status_coupa = gc_enviado OR <fs_dados>-status_coupa = gc_liberado.
      CONTINUE.
    ENDIF.

    " ---- a cada nova req.
    AT NEW banfn.

      CLEAR lv_count.

      CLEAR: lt_dados_envio, lt_inter_tab.


    ENDAT.

    " ---- a cada novo item.
    AT NEW bnfpo.

      APPEND INITIAL LINE TO lt_dados_envio ASSIGNING FIELD-SYMBOL(<fs_envio>).

      ADD 1 TO lv_count.

      READ TABLE gt_eban ASSIGNING FIELD-SYMBOL(<fs_eban>)
        WITH KEY banfn = <fs_dados>-banfn
                 bnfpo = <fs_dados>-bnfpo.

      IF sy-subrc EQ 0.

        IF <fs_eban>-afnam NE <fs_dados>-afnam.
          <fs_eban>-afnam = <fs_dados>-afnam.
        ENDIF.

        IF <fs_eban>-prio_urg NE <fs_dados>-prio_urg.
          <fs_eban>-prio_urg = <fs_dados>-prio_urg.
        ENDIF.

        MOVE-CORRESPONDING <fs_eban> TO <fs_envio>.

        PERFORM f_dados_envio_item USING <fs_eban> CHANGING <fs_envio>.
*BUG SOLTO 174842
        APPEND lw_dados TO gt_dados_aux.
*BUG SOLTO 174842
      ENDIF.

    ENDAT.

    " ---- a final da req.
    AT END OF banfn . "BUG SOLTO 174842
      lv_count = 99.
    ENDAT.


    " ---- a final da req.
    IF lv_count GE 99. "*BUG SOLTO 174842

*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Início de Alteração
      READ TABLE lt_dados_envio INTO DATA(wa_dados) INDEX 1.

      " Só realiza a integração, caso a ordem não estiver sendo processada
      " por outro usuário
      PERFORM f_bloquear_rc USING wa_dados-banfn
                            CHANGING lv_erro.

      IF lv_erro IS INITIAL.
*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Fim de Alteração

        APPEND INITIAL LINE TO lt_dados_retorno ASSIGNING FIELD-SYMBOL(<fs_dados_ret>).

        PERFORM f_envia_ordem USING lt_dados_envio.

        PERFORM f_envia_req_compra
          USING lt_dados_envio
                CHANGING lv_id_coupa
                         lv_status_coupa
                         lv_erro.

        IF lv_erro IS INITIAL.
          LOOP AT gt_dados_aux INTO DATA(wa_dados_aux). "*BUG SOLTO 174842
            PERFORM f_atualiza_ebban
                  USING wa_dados_aux-banfn
                        wa_dados_aux-bnfpo
                        lv_id_coupa
                        lv_status_coupa
                        <fs_dados>-afnam
                        <fs_dados>
                   CHANGING lv_erro.
          ENDLOOP.

        ENDIF.
        CLEAR lv_count.
        CLEAR: lt_dados_envio, lt_inter_tab.
        REFRESH gt_dados_aux.

*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Início de Alteração
        " Como a requisição foi bloqueado, agora realizar o desbloqueio
        PERFORM f_desbloquear_rc USING wa_dados-banfn
                                 CHANGING lv_erro.

      ENDIF.

      CLEAR wa_dados.
*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Fim de Alteração

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_ENVIO_ITEM
*&---------------------------------------------------------------------*
FORM f_dados_envio_item  USING    p_eban TYPE eban
                         CHANGING p_envio TYPE zmms_dados_int_coupa_eban.

  READ TABLE gt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>)
    WITH KEY werks = p_eban-werks.

  IF sy-subrc EQ 0.

    p_envio-t001w_name1 = <fs_t001w>-name1.

  ENDIF.

  READ TABLE gt_t001k ASSIGNING FIELD-SYMBOL(<fs_t001k>)
    WITH KEY bwkey = p_eban-werks.

  IF sy-subrc EQ 0.

    READ TABLE gt_t001 ASSIGNING FIELD-SYMBOL(<fs_t001>)
      WITH KEY bukrs = <fs_t001k>-bukrs.

    IF sy-subrc EQ 0.
      p_envio-t001_bukrs = <fs_t001>-bukrs.
      p_envio-t001_butxt = <fs_t001>-butxt.
    ENDIF.

  ENDIF.

  READ TABLE gt_ebkn ASSIGNING FIELD-SYMBOL(<fs_ebkn>)
    WITH KEY banfn = p_eban-banfn
             bnfpo =  p_eban-bnfpo.

  IF sy-subrc EQ 0.

    p_envio-ebkn_kostl = <fs_ebkn>-kostl.
    p_envio-ebkn_sakto = <fs_ebkn>-sakto.
    p_envio-ebkn_aufpl_ord = <fs_ebkn>-aufpl_ord.
    p_envio-ebkn_aplzl_ord = <fs_ebkn>-aplzl_ord.
    p_envio-ebkn_aufnr = <fs_ebkn>-aufnr.

    p_envio-objnr = 'OV' && <fs_ebkn>-aufpl && <fs_ebkn>-aplzl.

    READ TABLE gt_skat ASSIGNING FIELD-SYMBOL(<fs_skat>)
      WITH KEY saknr = <fs_ebkn>-sakto.

    IF sy-subrc EQ 0.

      p_envio-skat_ktopl = <fs_skat>-ktopl.
      p_envio-skat_saknr = <fs_skat>-saknr.
      p_envio-skat_txt50 = <fs_skat>-txt50.

      READ TABLE gt_cskt ASSIGNING FIELD-SYMBOL(<fs_cskt>)
        WITH KEY kostl = <fs_ebkn>-kostl.

      IF sy-subrc EQ 0.
        p_envio-cskt_ltext = <fs_cskt>-ltext.
      ENDIF.


    ENDIF.

    READ TABLE gt_npact ASSIGNING FIELD-SYMBOL(<fs_npact>)
      WITH KEY aufnr = <fs_ebkn>-aufnr
               objnr = p_envio-objnr.

    IF sy-subrc EQ 0.

      p_envio-v_npact_vornr = <fs_npact>-vornr.
      p_envio-v_npact_ltxa1 = <fs_npact>-ltxa1.

    ENDIF.

    READ TABLE gt_aufk ASSIGNING FIELD-SYMBOL(<fs_aufk>)
      WITH KEY aufnr = <fs_ebkn>-aufnr.

    IF sy-subrc EQ 0.
      p_envio-aufk_ktext = <fs_aufk>-ktext.
    ENDIF.

  ENDIF.

  READ TABLE gt_esll ASSIGNING FIELD-SYMBOL(<fs_esll>)
    WITH KEY packno = p_eban-packno.

  IF sy-subrc EQ 0.

    p_envio-esll_srvpos = <fs_esll>-srvpos.
    p_envio-esll_ktext1 = <fs_esll>-ktext1.
    p_envio-esll_extrow  = <fs_esll>-extrow.
    p_envio-esll_menge = <fs_esll>-menge.

    IF p_envio-esll_srvpos IS INITIAL.

      READ TABLE gt_esll_sub ASSIGNING FIELD-SYMBOL(<fs_esll_sub>)
        WITH KEY packno = <fs_esll>-sub_packno.

      IF sy-subrc EQ 0.

        p_envio-esll_srvpos = <fs_esll_sub>-srvpos.
        p_envio-esll_ktext1 = <fs_esll_sub>-ktext1.
        p_envio-esll_extrow  = <fs_esll_sub>-extrow.
        p_envio-esll_menge = <fs_esll_sub>-menge.

      ENDIF.

    ENDIF.


  ENDIF.

  PERFORM f_recupera_cab_text USING p_eban CHANGING p_envio-cab_txt.

  PERFORM f_retira_zeros
    CHANGING: p_envio-ebkn_aufnr,
              p_envio-ebkn_sakto,
              p_envio-matnr,
              p_envio-esll_srvpos,
              p_envio-ebkn_kostl.
  "p_envio-v_npact_vornr. " 11.05.2022 - RAMON - deve ir com zeros a esquerda

  PERFORM f_remove_char_special
    CHANGING: p_envio-t001_bukrs,
              p_envio-t001_butxt,
              p_envio-t001w_name1,
              p_envio-aufk_ktext,
              p_envio-v_npact_ltxa1,
              p_envio-skat_txt50,
              p_envio-esll_ktext1,
              p_envio-cskt_ltext.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RECUPERA_CAB_TEXT
*&---------------------------------------------------------------------*
FORM f_recupera_cab_text  USING    p_eban TYPE eban
                          CHANGING p_cab_txt TYPE string_data."tdline.

  DATA lv_name TYPE tdobname.
  DATA lt_lines TYPE TABLE OF tline.

  CLEAR p_cab_txt.

  lv_name = p_eban-banfn.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id       = 'B01'
      language = sy-langu
      name     = lv_name
      object   = 'EBANH'
    TABLES
      lines    = lt_lines
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CHECK lt_lines IS NOT INITIAL.

  p_cab_txt = lt_lines[ 1 ]-tdline.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RETIRA_ZEROS
*&---------------------------------------------------------------------*
FORM f_retira_zeros  CHANGING p_var TYPE c.

  CHECK p_var IS NOT INITIAL.

  SHIFT p_var LEFT DELETING LEADING '0'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REMOVE_CHAR_SPECIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_ENVIO_T001_BUKRS  text
*----------------------------------------------------------------------*
FORM f_remove_char_special CHANGING p_text TYPE c.

  DATA lv_text2 TYPE char100.

  lv_text2 = p_text.

  CALL FUNCTION 'ES_REMOVE_SPECIAL_CHARACTER'
    EXPORTING
      text1       = lv_text2
    IMPORTING
      corr_string = lv_text2.

  p_text = lv_text2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_EBBAN
*&---------------------------------------------------------------------*
FORM f_atualiza_ebban  USING p_banfn TYPE banfn
                             p_bnfpo TYPE bnfpo
                             p_id_coupa TYPE zemm_id_coupa
                             p_status_coupa TYPE zemm_status_coupa
                             p_afnam TYPE eban-afnam
                             p_dados TYPE zmms_dados_int_coupa_alv
                             p_erro TYPE c.

  CLEAR p_erro.

  IF p_status_coupa IS NOT INITIAL AND p_id_coupa IS NOT INITIAL.

*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Início de Alteração
*    "
*    call function 'ENQUEUE_EMEBANE'
*      exporting
*        mode_eban      = 'E'
*        mandt          = sy-mandt
*        banfn          = p_banfn
*        "bnfpo          = p_bnfpo
*        x_banfn        = ' '
*        x_bnfpo        = ' '
*        _scope         = '1'
*        _wait          = ' '
*        _collect       = ' '
*      exceptions
*        foreign_lock   = 1
*        system_failure = 2
*        others         = 3.


*    if sy-subrc <> 0.
*      p_erro = 'X'.
*      exit.
*    endif.
*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Fim de Alteração
*
    UPDATE eban SET id_coupa = p_id_coupa
                status_coupa = p_status_coupa
                afnam        = p_afnam
                prio_urg     = p_dados-prio_urg
                 WHERE banfn = p_banfn
                 AND   bnfpo = p_bnfpo. "BUG SOLTO 174842


  ENDIF.

  IF line_exists( gt_bapiret2[ type = 'E' ] ).

    ROLLBACK WORK.

    "PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

  ELSE.

    COMMIT WORK.

  ENDIF.


*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Início de Alteração
**desblolqueia RC
*  call function 'DEQUEUE_EMEBANE'
*    exporting
*      mode_eban      = 'E'
*      mandt          = sy-mandt
*      banfn          = p_banfn
*      "bnfpo          = p_eban-bnfpo
*      x_banfn        = ' '
*      x_bnfpo        = ' '
*      _scope         = '1'
*      _wait          = ' '
*      _collect       = ' '
*    exceptions
*      foreign_lock   = 1
*      system_failure = 2
*      others         = 3.
*
*  if sy-subrc <> 0.
*    "PERFORM f_mensagem_sistema_insere.
*    p_erro = 'X'.
*    exit.
*  endif.
*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Fim de Alteração

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_ORDEM
*&---------------------------------------------------------------------*
FORM f_envia_ordem  USING p_dados_tab TYPE zmmc_dados_int_coupa_eban.

  DATA(lt_om) = p_dados_tab.


  SORT lt_om BY ebkn_aufnr.

  DELETE lt_om WHERE ebkn_aufnr IS INITIAL.

  DELETE ADJACENT DUPLICATES FROM lt_om COMPARING ebkn_aufnr.

  LOOP AT lt_om ASSIGNING FIELD-SYMBOL(<fs_om>).

    READ TABLE gt_envio_om TRANSPORTING NO FIELDS WITH KEY id_integr = <fs_om>-ebkn_aufnr.

    " somente se nao existe na tabela
    CHECK sy-subrc NE 0.

    PERFORM f_sap_indicator USING 'Enviando ordem de manutenção...' 50.

    SUBMIT zmmr0035
            WITH s_lookup = 'OC'
            WITH p_op_obj = 'OM'
            WITH s_chave = <fs_om>-ebkn_aufnr
            WITH p_batch = 'X'  AND RETURN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_REQ_COMPRA
*&---------------------------------------------------------------------*
FORM f_envia_req_compra  USING    p_dados_tab  TYPE zmmc_dados_int_coupa_eban
                         CHANGING p_id_coupa   TYPE zemm_id_coupa
                                  p_stat_coupa TYPE zemm_status_coupa
                                  p_error      TYPE c.

  DATA lo_xml_ret TYPE REF TO cl_xml_document.

  CHECK p_dados_tab IS NOT INITIAL.

  CLEAR gt_bapiret2.

  p_error = 'X'. "<- se nao for erro, vai ser limpado

  TRY.
      CREATE OBJECT go_int
        EXPORTING
          i_servico = gc_service
          i_req     = p_dados_tab.

      IF go_int IS BOUND.

        DATA(lv_id_ret) = go_int->zif_integracao_coupa_req_comp~enviar_coupa( ).

        IF lv_id_ret CO '0123456789'.

          p_stat_coupa = gc_enviado.

          p_id_coupa = lv_id_ret.

          p_error = space.

        ENDIF.

      ENDIF.

    CATCH zcx_integracao INTO DATA(ex_int).

      DATA(lv_text) = ex_int->get_longtext( ).

      PERFORM f_mensagem_insere_txt USING 'E' lv_text.

    CATCH zcx_error INTO DATA(ex_erro).

      lv_text = ex_erro->get_longtext( ).

      PERFORM f_mensagem_insere_txt USING 'E' lv_text.

  ENDTRY.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_mensagem_insere_txt  USING i_type TYPE bapi_mtype
                                 p_string TYPE string.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 30
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        lv_msg1 = <fs_line>.
      WHEN 2.
        lv_msg2 = <fs_line>.
      WHEN 3.
        lv_msg3 = <fs_line>.
      WHEN 4.
        lv_msg4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  PERFORM f_mensagem_insere TABLES gt_bapiret2 USING i_type
                                                     'DS'
                                                     '016'
                                                     lv_msg1
                                                     lv_msg2
                                                     lv_msg3
                                                     lv_msg4.

ENDFORM.

*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Início de Alteração
FORM f_bloquear_rc USING    p_banfn  TYPE zmms_dados_int_coupa_eban-banfn
                   CHANGING p_error  TYPE c.

  CLEAR p_error.

  IF p_banfn IS NOT INITIAL.

    CALL FUNCTION 'ENQUEUE_EMEBANE'
      EXPORTING
        mode_eban      = 'E'
        mandt          = sy-mandt
        banfn          = p_banfn
        x_banfn        = ' '
        x_bnfpo        = ' '
        _scope         = '1'
        _wait          = ' '
        _collect       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.


    IF sy-subrc <> 0.

      p_error = 'X'.

    ENDIF.

  ELSE.

    p_error = 'X'.

  ENDIF.

ENDFORM.

FORM f_desbloquear_rc USING    p_banfn  TYPE zmms_dados_int_coupa_eban-banfn
                      CHANGING p_error  TYPE c.


  CALL FUNCTION 'DEQUEUE_EMEBANE'
    EXPORTING
      mode_eban      = 'E'
      mandt          = sy-mandt
      banfn          = p_banfn
      x_banfn        = ' '
      x_bnfpo        = ' '
      _scope         = '1'
      _synchron      = ' '
      _collect       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.

    p_error = 'X'.

  ENDIF.

ENDFORM.
*** Stefanini - IR233582 - 08/05/2025 - LAZAROSR - Fim de Alteração
