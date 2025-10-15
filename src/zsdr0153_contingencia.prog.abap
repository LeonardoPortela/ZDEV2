*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 22.05.2023                                              &*
*& Descrição: Processamento Gerar Lotes de vendas                     &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zsdr0153_contingencia.

************************************************************************
* tabelas
************************************************************************
TABLES: j_1bbranch, j_1bnflin, zsdt0001, j_1bnfe_active, zmmt0126, zsdt0330, zmmt0008.

************************************************************************
*& constantes
************************************************************************
CONSTANTS: c_s TYPE c VALUE 'S',
           c_0 TYPE c VALUE '0',
           c_1 TYPE c VALUE '1',
           c_x TYPE c VALUE 'X'.

************************************************************************
*& estruturas
************************************************************************
TYPES: BEGIN OF ty_zsdt0330.
         INCLUDE TYPE zsdt0330.
TYPES:   charg TYPE zmmt0008-charg.
TYPES:  lgortr TYPE zmmt0008-lgortr.
TYPES: END OF ty_zsdt0330.

TYPES: BEGIN OF ty_carga,
         ch_referencia TYPE zsdt0330-ch_referencia,
         nr_romaneio   TYPE zmmt0008-nr_romaneio,
         werks         TYPE zsdt0330-werks,
         lgort         TYPE zsdt0330-lgort,
         safra         TYPE zsdt0330-safra,
         kunnr         TYPE zsdt0330-kunnr,
         placa_cav     TYPE zsdt0330-placa_cav,
         motorista     TYPE char100.
TYPES: END OF ty_carga.

TYPES: BEGIN OF ty_zsdt0001,
         nr_romaneio TYPE zsdt0001-nr_romaneio,
         vbeln       TYPE zsdt0001-vbeln,
         branch      TYPE zsdt0001-branch,
         doc_rem     TYPE zsdt0001-doc_rem,
         id_cli_dest TYPE zsdt0001-id_cli_dest,
       END OF ty_zsdt0001,

       BEGIN OF ty_vbrp,
         vbeln  TYPE vbrp-vbeln,
         vgbel  TYPE vbrp-vgbel,
         matnr  TYPE vbrp-matnr,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_vbrp,

       BEGIN OF ty_lin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_lin,

       BEGIN OF ty_bnfe,
         docnum TYPE j_1bnfe_active-docnum,
         nfnum9 TYPE j_1bnfe_active-nfnum9,
         bukrs  TYPE j_1bnfe_active-bukrs,
         branch TYPE j_1bnfe_active-branch,
       END OF ty_bnfe,

       BEGIN OF ty_zmmt0008,
         werks              TYPE zmmt0008-werks,
         lgort              TYPE zmmt0008-lgort,
         nr_romaneio        TYPE zmmt0008-nr_romaneio,
         charg              TYPE zmmt0008-charg,
         menge              TYPE zmmt0008-menge,
         werks_orig         TYPE zmmt0008-werks_orig,
         lgortr             TYPE lgort_d,  "*-S4H-US 123905-25.09.2023-JT
         safra              TYPE charg_d,  "*-S4H-US 123905-25.09.2023-JT
         mblnr              TYPE mblnr,    "*-S4H-US 123905-25.09.2023-JT
         mjahr              TYPE mjahr,    "*-S4H-US 123905-25.09.2023-JT
         dados_contingencia TYPE char01,   "*-S4H-US 123905-25.09.2023-JT
       END OF ty_zmmt0008,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         normt TYPE mara-normt,
       END OF ty_mara,

       BEGIN OF ty_total,
         total TYPE sy-tabix,
         tipo  TYPE mara-normt,
         peso  TYPE zmmt0008-menge,
       END OF ty_total.

TYPES: BEGIN OF ty_fardos_exc,
         fardos  TYPE char21,
         peso    TYPE zmms004-peso,
         tipo    TYPE zmms004-tipo,
         tamanho TYPE zppt0004-tipo,
       END OF ty_fardos_exc.

TYPES: BEGIN OF ty_zsdt0045,
         zseq_inst     TYPE zsdt0045-zseq_inst,
         objek         TYPE zsdt0045-objek,
         objecttable   TYPE zsdt0045-objecttable,
         booking       TYPE zsdt0045-booking,
         instrucao     TYPE zsdt0045-instrucao,
         tamanho_fardo TYPE zsdt0045-tamanho_fardo,
       END OF ty_zsdt0045.

TYPES: BEGIN OF ty_excel,
         empresa    TYPE zmms005-empresa,
         safra      TYPE zmms005-safra,
         remetente  TYPE zmms005-remetente,
         terminal   TYPE zmms005-destinatario,
         cliente    TYPE zmms005-cliente,
         romaneio   TYPE zmms005-romaneio,
         nfnum      TYPE zmms005-nfnum,
         motorista  TYPE zmms005-motorista,
         veiculo    TYPE zmms005-veiculo,
         bloco      TYPE zmms005-bloco,
         qtd_fardos TYPE zmms006-total,
         peso_total TYPE zmms004-peso,
         instrucao  TYPE zsdt0066-instrucao,
         stcd1      TYPE j_1bbranch-stcd1,
       END OF ty_excel.

TYPES: BEGIN OF ty_saida,
         mark               TYPE c,
         werks              TYPE zmmt0008-werks,
         bloco              TYPE zmmt0008-lgort,
         fardo              TYPE zmmt0008-charg,
         nr_romaneio        TYPE zmmt0008-nr_romaneio,
         nfnum              TYPE zmmt0008-nfnum,
         vbeln              TYPE zmmt0008-vbeln,
         vbeln_vf           TYPE zmmt0008-vbeln_vf,
         placa_cav          TYPE zmmt0008-placa_cav,
         matnr              TYPE vbrp-matnr,
         menge              TYPE zmmt0008-menge,
         werks_orig         TYPE zmmt0008-werks_orig,
         ch_ref             TYPE zsdt0330-ch_referencia,
         lgortr             TYPE lgort_d,  "*-S4H-US 123905-25.09.2023-JT
         safra              TYPE charg_d,  "*-S4H-US 123905-25.09.2023-JT
         mblnr              TYPE mblnr,    "*-S4H-US 123905-25.09.2023-JT
         mjahr              TYPE mjahr,    "*-S4H-US 123905-25.09.2023-JT
         dados_contingencia TYPE char01,   "*-S4H-US 123905-25.09.2023-JT
       END OF ty_saida.

************************************************************************
*& variaveis globais
************************************************************************
DATA: t_zsdt0330      TYPE TABLE OF ty_zsdt0330,
      w_zsdt0330      TYPE ty_zsdt0330,
      t_zmmt0008      TYPE TABLE OF zmmt0008,
      w_zmmt0008      TYPE zmmt0008,
      t_erro          TYPE TABLE OF zmmt0008,
      w_erro          TYPE zmmt0008,
      t_zsdt0330_grp  TYPE TABLE OF ty_zsdt0330,
      t_carga         TYPE TABLE OF ty_carga,
      t_lote          TYPE TABLE OF ty_zsdt0330,
      w_lote          TYPE ty_zsdt0330,
      t_status        TYPE zde_btcstatus_t,
      l_tabix         TYPE sy-tabix,
      l_mensagem      TYPE string,
      l_jobs          TYPE i,
      l_erro          TYPE char01,
      w_zsdt0330_grp  TYPE ty_zsdt0330,
      it_zmail        TYPE STANDARD TABLE OF zmail,
      wa_zmail        TYPE zmail,
*
      w_carga         TYPE ty_carga,
      tg_zsdt0001     TYPE TABLE OF ty_zsdt0001,
      wg_zsdt0001     TYPE ty_zsdt0001,
      tg_vbrp         TYPE TABLE OF ty_vbrp,
      wg_vbrp         TYPE ty_vbrp,
      tg_lin          TYPE TABLE OF ty_lin,
      wg_lin          TYPE ty_lin,
      tg_doc          TYPE TABLE OF j_1bnfdoc,
      wg_doc          TYPE j_1bnfdoc,
      tg_bnfe         TYPE TABLE OF ty_bnfe,
      wg_bnfe         TYPE ty_bnfe,
      tg_saida        TYPE TABLE OF ty_saida,
      wg_saida        TYPE ty_saida,
      tg_zmmt0008     TYPE TABLE OF ty_zmmt0008,
      tg_zmmt0008_aux TYPE TABLE OF ty_zmmt0008,
      wg_zmmt0008     TYPE ty_zmmt0008,
      tg_kna1         TYPE TABLE OF ty_kna1,
      wg_kna1         TYPE ty_kna1,
      wg_header       TYPE zmms005,
      tg_fardos       TYPE TABLE OF zmms004 WITH HEADER LINE,
      tg_mara         TYPE TABLE OF ty_mara,
      wg_mara         TYPE ty_mara,
      tg_excel        TYPE TABLE OF ty_excel,
      wg_excel        TYPE ty_excel,
      wa_zsdt0045     TYPE ty_zsdt0045,
      tg_fardos_exc   TYPE TABLE OF ty_fardos_exc,
      wg_fardos_exc   TYPE ty_fardos_exc,
      v_name(30),
      v_input,
      v_required,
      v_request,
      v_value,
      v_txtbtn        TYPE c LENGTH 50,
      v_init          TYPE c,
      l_mesg1         TYPE char200,
      l_mesg2         TYPE char200,
      l_mesg3         TYPE char200,
*
      send_request    TYPE REF TO cl_bcs,
      document        TYPE REF TO cl_document_bcs,
      recipient       TYPE REF TO if_recipient_bcs,
      bcs_exception   TYPE REF TO cx_bcs,
*
      main_text       TYPE bcsy_text,
      binary_content  TYPE solix_tab,
      size            TYPE so_obj_len,
      sent_to_all     TYPE os_boolean,
*
      l_program       TYPE sy-repid,
      l_grid_title    TYPE lvc_title,
      w_layout        TYPE slis_layout_alv,
      t_fieldcat      TYPE slis_t_fieldcat_alv.

************************************************************************
*  parametro ID_CARGA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS: s_werks FOR zmmt0008-werks,
                  s_lgort FOR zmmt0008-lgort,
                  s_charg FOR zmmt0008-charg.
SELECTION-SCREEN END   OF BLOCK b1.

************************************************************************
*&      Start-Of-Selection
************************************************************************
START-OF-SELECTION.

*---------------------------------------------
*-Processamento
*---------------------------------------------
  PERFORM f_selecao_fardos.

************************************************************************
*&-selecao dos fardos
************************************************************************
FORM f_selecao_fardos.

  FREE: t_carga.

*------------------------------------------
*- recupera ch_referencia
*------------------------------------------
  SELECT *
    FROM zmmt0008
    INTO CORRESPONDING FIELDS OF TABLE t_zsdt0330
   WHERE werks              IN s_werks
     AND lgort              IN s_lgort
     AND charg              IN s_charg
     AND nr_romaneio        <> '000000000'
     AND vbeln_vf            = abap_off
     AND dados_contingencia  = abap_true.

  SELECT *
    FROM zmmt0008
    INTO TABLE t_zmmt0008
   WHERE werks              IN s_werks
     AND lgort              IN s_lgort
     AND charg              IN s_charg
     AND nr_romaneio        <> '000000000'
     AND vbeln_vf            = abap_off
     AND dados_contingencia  = abap_true.

  LOOP AT t_zsdt0330  INTO w_zsdt0330.
    w_zsdt0330-id_carga  = '00001'.
    w_zsdt0330-acharg    = w_zsdt0330-charg.
    MODIFY t_zsdt0330 FROM w_zsdt0330 INDEX sy-tabix.
  ENDLOOP.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.
    l_tabix = sy-tabix.

    SELECT SINGLE  ch_referencia
      INTO @DATA(l_ch_referencia)
      FROM zsdt0001
     WHERE tp_movimento = 'S'
       AND nr_romaneio  = @w_zsdt0330-nr_romaneio
       AND vbeln        = @w_zsdt0330-vbeln.

    CHECK sy-subrc = 0.

    w_zsdt0330-ch_referencia = l_ch_referencia.
    MODIFY t_zsdt0330     FROM w_zsdt0330 INDEX l_tabix.
  ENDLOOP.

  CHECK t_zsdt0330[] IS NOT INITIAL.

  t_zsdt0330_grp[] = t_zsdt0330[].

*---------------------------------
* agrupa por centro + deposito + safra
*---------------------------------
  SORT t_zsdt0330_grp BY ch_referencia safra werks lgort.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0330_grp
                        COMPARING ch_referencia safra werks lgort.

*---------------------------------
* processamento
*---------------------------------
  LOOP AT t_zsdt0330_grp INTO w_zsdt0330_grp.
    FREE: t_carga, t_lote.

    MOVE-CORRESPONDING w_zsdt0330_grp TO w_carga.

    SELECT SINGLE nr_romaneio
      FROM zsdt0001
      INTO @DATA(_nr_romaneio)
     WHERE tp_movimento  = @c_s
       AND ch_referencia = @w_zsdt0330_grp-ch_referencia
       AND branch        = @w_zsdt0330_grp-werks
       AND nr_safra      = @w_zsdt0330_grp-safra.

    IF sy-subrc <> 0.
      l_mensagem   = 'Romaneio informado não foi encontrado'.
      PERFORM f_gera_erro_carga USING '2' l_mensagem.
      CONTINUE.
    ENDIF.

    MOVE _nr_romaneio                 TO w_carga-nr_romaneio.

*-----------------------------
* -lotes da carga
*-----------------------------
    LOOP AT t_zsdt0330 INTO w_zsdt0330 WHERE ch_referencia = w_zsdt0330_grp-ch_referencia
                                         AND safra         = w_zsdt0330_grp-safra
                                         AND werks         = w_zsdt0330_grp-werks
                                         AND lgort         = w_zsdt0330_grp-lgort.
      APPEND w_zsdt0330  TO t_lote.
    ENDLOOP.

    APPEND w_carga                    TO t_carga.

    PERFORM f_carrega_outros_dados.
    PERFORM f_selecao_dados CHANGING l_erro.

    CHECK l_erro = abap_off.

    PERFORM f_organiza_dados.
*   PERFORM f_atualiza_zmmt0008.

    l_mensagem = 'Geração de Lote finalizada. verifique ZMMT0008'.
    MESSAGE s024(sd) WITH l_mensagem.

    COMMIT WORK AND WAIT.

  ENDLOOP.

ENDFORM.

************************************************************************
*&-gerar erro para o fardo
************************************************************************
FORM f_gera_erro_carga USING p_status
                             p_mensagem.

  MESSAGE i024(sd) WITH p_mensagem.

ENDFORM.

************************************************************************
*&-carregar outras informacoes
************************************************************************
FORM f_carrega_outros_dados.

  READ TABLE t_carga INTO w_carga INDEX 1.

  SELECT vbeln, id_cli_dest, placa_cav, motorista
    INTO TABLE @DATA(t_0001)
    FROM zsdt0001
   WHERE ch_referencia = @w_carga-ch_referencia.
*    AND nr_romaneio   = @w_carga-nr_romaneio
*    AND nr_safra      = @w_carga-safra
*    AND branch        = @w_carga-werks.

  IF sy-subrc = 0.
    READ TABLE t_0001 INTO DATA(w_0001) INDEX 1.

    IF sy-subrc = 0.
      w_carga-placa_cav   = w_0001-placa_cav.

      SELECT SINGLE name1
        INTO @DATA(l_name1)
        FROM lfa1
       WHERE lifnr = @w_0001-motorista.
      IF sy-subrc = 0.
        w_carga-motorista = l_name1.
      ENDIF.
    ENDIF.

    LOOP AT t_0001 INTO w_0001.
      SELECT SINGLE auart
        INTO @DATA(l_auart)
        FROM vbak
       WHERE vbeln = @w_0001-vbeln.

      IF sy-subrc = 0 AND l_auart <> 'ZRFL'.
        w_carga-kunnr     = w_0001-id_cli_dest.
*     ELSE.
*       CLEAR w_carga-kunnr.
      ENDIF.
    ENDLOOP.

    MODIFY t_carga FROM w_carga INDEX 1.
  ENDIF.

ENDFORM.

************************************************************************
*&-selecao dos fardos
************************************************************************
FORM f_selecao_dados CHANGING p_erro.

  CLEAR p_erro.

  READ TABLE t_carga INTO w_carga INDEX 1.

*------------------------------------------
*- selecao dos fardos a processar
*------------------------------------------
  SELECT nr_romaneio vbeln branch doc_rem id_cli_dest
    FROM zsdt0001
    INTO TABLE tg_zsdt0001
   WHERE tp_movimento  EQ c_s
     AND ch_referencia EQ w_carga-ch_referencia
     AND branch        EQ w_carga-werks
     AND nr_safra      EQ w_carga-safra.

  CHECK  sy-subrc = 0.

  SELECT vbrp~vbeln vgbel matnr
    FROM vbrp
    INNER JOIN vbrk ON vbrk~vbeln = vbrp~vbeln
    INTO TABLE tg_vbrp
     FOR ALL ENTRIES IN tg_zsdt0001
   WHERE vgbel EQ tg_zsdt0001-doc_rem
     AND fksto EQ abap_off.

  IF sy-subrc IS INITIAL.
    LOOP AT tg_vbrp  INTO wg_vbrp.
      wg_vbrp-refkey    = wg_vbrp-vbeln.
      MODIFY tg_vbrp FROM wg_vbrp.
    ENDLOOP.

    SELECT docnum refkey
      FROM j_1bnflin
      INTO TABLE tg_lin
       FOR ALL ENTRIES IN tg_vbrp
     WHERE refkey EQ tg_vbrp-refkey.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM j_1bnfdoc
        INTO TABLE tg_doc
         FOR ALL ENTRIES IN tg_lin
       WHERE docnum EQ tg_lin-docnum.

      SELECT docnum nfnum9 bukrs branch
        FROM j_1bnfe_active
        INTO TABLE tg_bnfe
         FOR ALL ENTRIES IN tg_lin
       WHERE docnum EQ tg_lin-docnum
         AND nfnum9 <> ' '.

      IF tg_bnfe[] IS INITIAL.
        p_erro     = abap_true.
        l_mensagem = 'Para Gerar o lote de Venda a NF-e do Romaneio informado deve estar Determinada'.
        PERFORM f_gera_erro_carga USING '2' l_mensagem.
        EXIT.
      ENDIF.
    ENDIF.
  ELSE.
    p_erro     = abap_true.
    l_mensagem   = 'Remessa/Fatura ainda não foram gerados!'.
    PERFORM f_gera_erro_carga USING '2' l_mensagem.
    EXIT.
  ENDIF.

*  IF p_email = abap_off AND p_integ = abap_off.
*    SELECT werks lgort nr_romaneio charg menge werks_orig
*      FROM zmmt0008
*      INTO TABLE tg_zmmt0008
*       FOR ALL ENTRIES IN t_lote
*     WHERE   werks       EQ w_carga-werks
*       AND   lgort       EQ w_carga-lgort
*       AND   charg       EQ t_lote-acharg
*       AND ( nr_romaneio EQ space OR
*             nr_romaneio EQ c_0  ).
*  ELSE.
  SELECT werks lgort nr_romaneio charg menge werks_orig
         lgortr safra mblnr mjahr dados_contingencia  "*-S4H-US 123905-25.09.2023-JT
    FROM zmmt0008
    INTO TABLE tg_zmmt0008
     FOR ALL ENTRIES IN t_lote
   WHERE   werks       EQ w_carga-werks
     AND   lgort       EQ w_carga-lgort
     AND   charg       EQ t_lote-acharg
     AND   nr_romaneio EQ w_carga-nr_romaneio.
*  ENDIF.

  IF tg_zmmt0008[] IS INITIAL.
    DATA: lva_count TYPE i.

    SELECT werks lgort nr_romaneio charg menge werks_orig
      FROM zmmt0008
      INTO TABLE tg_zmmt0008_aux
     WHERE werks       EQ w_carga-werks
       AND lgort       EQ w_carga-lgort
       AND nr_romaneio EQ w_carga-nr_romaneio
       AND placa_cav   EQ w_carga-placa_cav
       AND kunnr       EQ w_carga-kunnr
       AND status      EQ '3'.

    DESCRIBE TABLE tg_zmmt0008_aux LINES lva_count.

    IF lva_count > 0.
      p_erro     = abap_true.
      l_mensagem = 'Romaneio está em processo de cancelamento' &&
                   ' e estorno no sistema Rondonline,'         &&
                   ' por favor aguardar!'.
      PERFORM f_gera_erro_carga USING '2' l_mensagem.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT kunnr name1
    FROM kna1
    INTO TABLE tg_kna1
     FOR ALL ENTRIES IN tg_zsdt0001
   WHERE kunnr EQ tg_zsdt0001-id_cli_dest.

ENDFORM.

************************************************************************
* organizar dados
************************************************************************
FORM f_organiza_dados.

  FREE: tg_saida, wg_saida, t_erro.

  READ TABLE t_carga INTO w_carga INDEX 1.

  LOOP AT tg_vbrp INTO wg_vbrp.

    CLEAR: wg_lin, wg_bnfe, wg_zsdt0001.

    READ TABLE tg_lin INTO wg_lin WITH KEY refkey = wg_vbrp-refkey.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bnfe     INTO wg_bnfe     WITH KEY docnum  = wg_lin-docnum.
      CHECK sy-subrc = 0.

*     READ TABLE tg_doc      INTO wg_doc      WITH KEY docnum  = wg_lin-docnum.
*     CHECK sy-subrc = 0.

      READ TABLE tg_zsdt0001 INTO wg_zsdt0001 WITH KEY doc_rem = wg_vbrp-vgbel.

      LOOP AT tg_zmmt0008          INTO wg_zmmt0008.
        READ TABLE t_zsdt0330      INTO w_zsdt0330 WITH KEY werks  = wg_zmmt0008-werks
                                                            lgort  = wg_zmmt0008-lgort
                                                            acharg = wg_zmmt0008-charg.
        CHECK sy-subrc = 0.

        READ TABLE t_zmmt0008      INTO w_zmmt0008 WITH KEY werks = wg_zmmt0008-werks
                                                            lgort = wg_zmmt0008-lgort
                                                            charg = wg_zmmt0008-charg.
        IF w_zmmt0008-nfnum <> wg_bnfe-nfnum9.
          MOVE wg_bnfe-nfnum9        TO w_zmmt0008-vbeln_vf.
          APPEND w_zmmt0008 TO t_erro.
        ELSE.
*         MOVE: abap_true              TO wg_saida-mark,
*               w_zmmt0008-werks      TO wg_saida-werks,
*               w_zmmt0008-lgort      TO wg_saida-bloco,
*               w_zmmt0008-charg      TO wg_saida-fardo,
*               w_zmmt0008-nr_romaneio TO wg_saida-nr_romaneio,
*               w_zmmt0008-nfnum      TO wg_saida-nfnum,
*               w_zmmt0008-vbeln      TO wg_saida-vbeln,
          MOVE wg_vbrp-vbeln         TO w_zmmt0008-vbeln_vf.
*               w_zmmt0008-matnr      TO wg_saida-matnr,
*               w_carga-placa_cav     TO wg_saida-placa_cav,
**              wg_zmmt0008-ch_referencia  TO wg_saida-ch_ref,
*               w_zmmt0008-menge      TO wg_saida-menge,
*               w_zmmt0008-werks_orig TO wg_saida-werks_orig,
*               w_zmmt0008-lgortr     TO wg_saida-lgortr, "*-S4H-US 123905-25.09.2023-JT
*               w_zmmt0008-safra      TO wg_saida-safra,  "*-S4H-US 123905-25.09.2023-JT
*               w_zmmt0008-mblnr      TO wg_saida-mblnr,  "*-S4H-US 123905-25.09.2023-JT
*               w_zmmt0008-mjahr      TO wg_saida-mjahr,  "*-S4H-US 123905-25.09.2023-JT
*               w_zmmt0008-dados_contingencia TO wg_saida-dados_contingencia. "*-S4H-US 123905-25.09.2023-JT

          MODIFY zmmt0008 FROM w_zmmt0008.

*        APPEND wg_saida              TO tg_saida.
*        CLEAR: wg_saida.
        ENDIF.
      ENDLOOP.

      CLEAR: wg_vbrp, wg_lin, wg_zsdt0001, wg_bnfe.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.

  CHECK t_erro[] IS NOT INITIAL.

  l_program                  = sy-repid.
  l_grid_title               = 'Linhas não modificadas - Notas Fiscais Divergentes ECC x Hana'.
  w_layout-expand_all        = abap_true.
  w_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = l_program
      i_structure_name       = 'ZMMT0008'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = l_program
      is_layout             = w_layout
      it_fieldcat           = t_fieldcat
      i_grid_title          = l_grid_title
      i_screen_start_column = 10
      i_screen_start_line   = 02
      i_screen_end_column   = 182
      i_screen_end_line     = 20
    TABLES
      t_outtab              = t_erro
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

ENDFORM.

************************************************************************
* atualiza zmmt0008
************************************************************************
FORM f_atualiza_zmmt0008.

  DATA: tl_0008        TYPE TABLE OF zmmt0008 WITH HEADER LINE,
        w_werks        TYPE zmmt0008-werks,
        l_interc       TYPE c,
        wl_cont        TYPE sy-tabix,
        msg(150),
        resposta,
        wl_cont_aux(6).

  TYPES: tt_selection TYPE STANDARD TABLE OF rsparams.

  DATA: l_selection  TYPE rsparams,
        li_selection TYPE tt_selection.

  READ TABLE t_carga  INTO w_carga  INDEX 1.

  READ TABLE tg_saida INTO wg_saida WITH KEY mark  = c_x
                                             nfnum = 0.
  IF sy-subrc IS INITIAL.
    l_mensagem = 'Não localizado número da Nota para o romaneio informado,' &&
                 'verifique se a mesma esta determinada!'.
    PERFORM f_gera_erro_carga USING '2' l_mensagem.
    EXIT.
  ENDIF.

  SELECT SINGLE kunnr, ktokd
    FROM kna1
    INTO @DATA(w_kna1)
   WHERE kunnr = @w_carga-kunnr.

  IF sy-subrc = 0 AND w_kna1-ktokd = 'ZCIC'.
    l_interc = abap_true.

    IF tg_saida[] IS NOT INITIAL.
      SELECT *
        FROM zmmt0008
        INTO TABLE @DATA(t_0008)
         FOR ALL ENTRIES IN @tg_saida
       WHERE werks = @w_kna1-kunnr+6(4)
         AND lgort = @tg_saida-bloco
         AND charg = @tg_saida-fardo.
    ENDIF.
  ENDIF.

*------------------------------------------
* atualizar zmmt0008
*------------------------------------------
  LOOP AT tg_saida INTO wg_saida  WHERE mark EQ c_x.

    CLEAR tl_0008.

    READ TABLE t_zmmt0008      INTO w_zmmt0008 WITH KEY werks = wg_saida-werks
                                                        lgort = wg_saida-bloco
                                                        charg = wg_saida-fardo.

    MOVE: wg_saida-werks       TO w_werks.
    MOVE: sy-mandt             TO tl_0008-mandt,
          wg_saida-werks       TO tl_0008-werks,
          wg_saida-bloco       TO tl_0008-lgort,
          wg_saida-fardo       TO tl_0008-charg,
          w_zmmt0008-nr_romaneio TO tl_0008-nr_romaneio,
          w_zmmt0008-nfnum     TO tl_0008-nfnum,
          w_zmmt0008-vbeln     TO tl_0008-vbeln,
          wg_saida-vbeln_vf    TO tl_0008-vbeln_vf,
          w_zmmt0008-placa_cav TO tl_0008-placa_cav,
          w_zmmt0008-menge     TO tl_0008-menge,
          w_zmmt0008-werks_orig TO tl_0008-werks_orig,
          w_carga-motorista    TO tl_0008-motorista,
          w_carga-kunnr        TO tl_0008-kunnr,
          wg_saida-matnr       TO tl_0008-matnr, "*-S4H-US 123905-25.09.2023-JT
          wg_saida-lgortr      TO tl_0008-lgortr, "*-S4H-US 123905-25.09.2023-JT
          wg_saida-safra       TO tl_0008-safra,  "*-S4H-US 123905-25.09.2023-JT
          wg_saida-mblnr       TO tl_0008-mblnr,  "*-S4H-US 123905-25.09.2023-JT
          wg_saida-mjahr       TO tl_0008-mjahr,  "*-S4H-US 123905-25.09.2023-JT
          w_zmmt0008-dt_inicial_integ  TO tl_0008-dt_inicial_integ,
          w_zmmt0008-status    TO tl_0008-status.

    APPEND tl_0008.

    IF l_interc = abap_true.
      READ TABLE t_0008 INTO DATA(w_0008) WITH KEY werks = w_kna1-kunnr+6(4)
                                                   lgort = wg_saida-bloco
                                                   charg = wg_saida-fardo.
      IF ( sy-subrc <> 0 ) OR ( sy-subrc = 0 AND w_0008-nr_romaneio IS INITIAL ).
*        CLEAR tl_0008.
*        MOVE:  sy-mandt          TO tl_0008-mandt,
*               w_kna1-kunnr+6(4) TO tl_0008-werks,
*               wg_saida-werks    TO tl_0008-werks_orig, "*-CS2022000332-#78803-05.07.2022-JT-inicio
*               wg_saida-bloco    TO tl_0008-lgort,
*               wg_saida-fardo    TO tl_0008-charg,
*               wg_saida-menge    TO tl_0008-menge,
*               wg_saida-matnr    TO tl_0008-matnr, "*-S4H-US 123905-25.09.2023-JT
*               wg_saida-lgortr   TO tl_0008-lgortr, "*-S4H-US 123905-25.09.2023-JT
*               wg_saida-safra    TO tl_0008-safra,  "*-S4H-US 123905-25.09.2023-JT
**              wg_saida-mblnr    TO tl_0008-mblnr,  "*-S4H-US 123905-25.09.2023-JT
**              wg_saida-mjahr    TO tl_0008-mjahr,  "*-S4H-US 123905-25.09.2023-JT
*               wg_saida-dados_contingencia TO tl_0008-dados_contingencia. "*-S4H-US 123905-25.09.2023-JT
*        APPEND tl_0008.
      ENDIF.
    ENDIF.

  ENDLOOP.

  MODIFY zmmt0008 FROM TABLE tl_0008.

*-----------------------------
*---atualiza status
*-----------------------------
  l_mensagem = 'Geração de Lote efetuada com Sucesso'.
  MESSAGE s024(sd) WITH l_mensagem.

  COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
************************************************************************
