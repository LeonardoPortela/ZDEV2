*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 24/07/2013                                              &*
*& Descrição: Lançamentos Manuais – Relatório de Lote	                &*
*& Transação: ZGL020                                                  &*
*---------------------------------------------------------------------&*
REPORT  zgl018.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zimp_cad_depto, zglt034, zglt036, zimp_aprovador.

TYPES: BEGIN OF ty_zib_contabil.
         INCLUDE STRUCTURE zib_contabil.
TYPES:   mark TYPE c,
       END OF ty_zib_contabil.

"tables : ZGLT034.
TYPES:
  BEGIN OF ty_zglt034,
    lote        TYPE zglt034-lote,
    descr_lote  TYPE zglt034-descr_lote,
    bukrs       TYPE zglt034-bukrs,
    usnam       TYPE zglt034-usnam,
    status_lote TYPE zglt034-status_lote,
    dep_resp    TYPE zglt034-dep_resp,
    tcode       TYPE zglt034-tcode,
    data_atual  TYPE zglt034-data_atual,
  END OF ty_zglt034,


  BEGIN OF ty_zgl34_fbb1,
    lote        TYPE faglflexa-docnr,
    descr_lote  TYPE zglt034-descr_lote,
    bukrs       TYPE zglt034-bukrs,
    usnam       TYPE zglt034-usnam,
    status_lote TYPE zglt034-status_lote,
    dep_resp    TYPE zglt034-dep_resp,
    tcode       TYPE zglt034-tcode,
    data_atual  TYPE zglt034-data_atual,
    usuario     TYPE zglt034-usuario,
  END OF ty_zgl34_fbb1,



  BEGIN OF ty_texto,
    name_empresa TYPE t001-butxt,
    descr_lote   TYPE zglt034-descr_lote,
  END OF ty_texto,

  " Lançamento Manuais
  BEGIN OF ty_zglt035,
    doc_lcto       TYPE  zglt035-doc_lcto,
    bukrs          TYPE  zglt035-bukrs,
    tp_lcto        TYPE  zglt035-tp_lcto,
    lote           TYPE  zglt035-lote,
    moeda_doc      TYPE  zglt035-moeda_doc,
    st_lc_moeda    TYPE  zglt035-st_lc_moeda,
    bldat          TYPE  zglt035-bldat,
    budat          TYPE  zglt035-budat,
    dt_lcto        TYPE  zglt035-dt_lcto,
    taxa           TYPE  zglt035-taxa,
    belnr          TYPE  zglt035-belnr,
    dpto_resp      TYPE  zglt035-dpto_resp,
    lote_prec      TYPE  zglt035-lote_prec,
    doc_lcto_prec  TYPE  zglt035-doc_lcto_prec,
    belnr_prec     TYPE  zglt035-belnr_prec,
    doc_contabil   TYPE  zib_contabil_chv-belnr, " contabil
    gjahr          TYPE  zib_contabil_chv-gjahr,
    doc_contabil_e TYPE  zib_contabil_chv-belnr, " contabil estorno
    gjahr_e        TYPE  zib_contabil_chv-gjahr,
    augbl          TYPE  bsak-augbl,
    flg_err(1),
    reversao_doc   TYPE zglt035-belnr,
  END OF ty_zglt035,

  BEGIN OF ty_zglt036,
    doc_lcto        TYPE zglt036-doc_lcto,
    seqitem         TYPE zglt036-seqitem,
    seqsub          TYPE zglt036-seqsub,
    tp_lcto         TYPE zglt036-tp_lcto,
    bschl           TYPE zglt036-bschl,
    hkont           TYPE zglt036-hkont,
    umskz           TYPE zglt036-umskz,
    anbwa           TYPE zglt036-anbwa,
    bewar           TYPE zglt036-bewar,
    vbund           TYPE zglt036-vbund,
    kostl           TYPE zglt036-kostl,
    prctr           TYPE zglt036-prctr,
    aufnr           TYPE zglt036-aufnr,
    matnr           TYPE zglt036-matnr,
    zuonr           TYPE zglt036-zuonr,
    sgtxt           TYPE zglt036-sgtxt,
    gsber           TYPE zglt036-gsber,
    vlr_moeda_doc   TYPE zglt036-vlr_moeda_doc,
    vlr_moeda_int   TYPE zglt036-vlr_moeda_int,
    vlr_moeda_forte TYPE zglt036-vlr_moeda_forte,
    vlr_moeda_grupo TYPE zglt036-vlr_moeda_doc,
  END OF ty_zglt036,

  BEGIN OF ty_zglt031,
    tp_lcto   TYPE zglt031-tp_lcto,
    descricao TYPE zglt031-descricao,
  END OF ty_zglt031,

  BEGIN OF ty_tbsl,
    bschl TYPE tbsl-bschl,
    shkzg TYPE tbsl-shkzg,
  END OF ty_tbsl,


  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs,
    butxt TYPE t001-butxt,
    land1 TYPE t001-land1,
  END OF ty_t001,


  BEGIN OF ty_tcurr,
    kurst TYPE tcurr-kurst,
    fcurr TYPE tcurr-fcurr,
    tcurr TYPE tcurr-tcurr,
    gdatu TYPE tcurr-gdatu,
    ukurs TYPE tcurr-ukurs,
  END OF ty_tcurr,

  BEGIN OF ty_t005,
    land1 TYPE t005-land1,
    waers TYPE t005-waers,
  END OF   ty_t005,


  BEGIN OF ty_zib_contabil_chv,
    obj_key TYPE zib_contabil_chv-obj_key,
    belnr   TYPE zib_contabil_chv-belnr,
    bukrs   TYPE zib_contabil_chv-bukrs,
    gjahr   TYPE zib_contabil_chv-gjahr,
  END OF ty_zib_contabil_chv,

  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    belnr TYPE bkpf-belnr,
    gjahr TYPE bkpf-gjahr,
    budat TYPE bkpf-budat,
    stblg TYPE bkpf-stblg,
    stjah TYPE bkpf-stjah,
  END OF ty_bkpf,

  BEGIN OF ty_bsak,
    bukrs TYPE bsak-bukrs,
    belnr TYPE bsak-belnr,
    gjahr TYPE bsak-gjahr,
    augbl TYPE bsak-augbl,
  END OF ty_bsak,

  BEGIN OF ty_zglt038,
    bukrs      TYPE zglt038-bukrs,
    lote       TYPE zglt038-lote,
    nivel      TYPE zglt038-nivel,
    aprovador  TYPE zglt038-aprovador,
    valor_de   TYPE zglt038-valor_de,
    valor_ate  TYPE zglt038-valor_ate,
    data_atual TYPE zglt038-data_atual,
    hora_atual TYPE zglt038-hora_atual,
    usuario    TYPE zglt038-usuario,
  END OF ty_zglt038,

  BEGIN OF ty_estra ,
    bukrs     TYPE zglt038-bukrs,
    lote      TYPE zglt038-lote,
    valor_de  TYPE zglt037-valor_de,
    valor_ate TYPE zglt037-valor_ate,
    aprovador TYPE zglt037-aprovador,
    nivel     TYPE zglt037-nivel,
    waers     TYPE zglt037-waers,
    estado(4),
    opcoes(4),
  END OF ty_estra,

  BEGIN OF ty_saida,
    icon(4)         TYPE c,
    aprov(20),
    doc_contabil    TYPE zib_contabil_chv-belnr, " contabil
    doc_contabil_e  TYPE zib_contabil_chv-belnr, " contabil estorno
    reversao_doc    TYPE zib_contabil_chv-belnr, "130130 - CS2023000969 Gisele Follmann PSA
    augbl           TYPE bsak-augbl,
    lote            TYPE zglt035-lote, " Lote.
    doc_lcto        TYPE zglt035-doc_lcto, " Doc.Imp.
    tp_lcto         TYPE zglt035-tp_lcto,
    descricao       TYPE zglt031-descricao,
    vlr_moeda_doc   TYPE zglt036-vlr_moeda_doc,
    vlr_moeda_int   TYPE zglt036-vlr_moeda_int,
    vlr_moeda_forte TYPE zglt036-vlr_moeda_forte,
    vlr_moeda_grupo TYPE zglt036-vlr_moeda_grupo,
    vlr_total       TYPE zglt036-vlr_moeda_grupo,
    vlr_total_lote  TYPE zglt036-vlr_moeda_grupo,
    bukrs           TYPE zib_contabil_chv-bukrs,
    gjahr           TYPE zib_contabil_chv-gjahr,
    gjahr_e         TYPE zib_contabil_chv-gjahr,
    budat           TYPE bkpf-budat,
    descr_lote      TYPE zglt034-descr_lote,
    usnam           TYPE zglt034-usnam,
    tcode           TYPE zglt034-tcode,
    dep_resp        TYPE zimp_cad_depto-dep_resp,
    dep_resp_desc   TYPE zimp_cad_depto-dep_resp_desc,
    lote_prec       TYPE zglt035-lote_prec,
    doc_lcto_prec   TYPE zglt035-doc_lcto_prec,
    belnr_prec      TYPE zglt035-belnr_prec,
  END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

**********************************************************************
* Range (Empresa Bukrs) Valida Usuário Objeto 110906 Range Filiais Perfil Acxesso - PSA
**********************************************************************
TYPES : BEGIN OF emp_out,
          bukrs        TYPE t001-bukrs,
          acesso(1)    TYPE c,
          message(255) TYPE c,
        END OF emp_out.




DATA: aux_empresa TYPE t001-bukrs,
      it_acesso   TYPE TABLE OF emp_out,
      _empresa    TYPE emp_out,
      _bukrs      TYPE emp_out,
      erro_acesso TYPE c.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: t_bdc               TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_messtab           TYPE TABLE OF bdcmsgcoll,
      tg_estra            TYPE TABLE OF ty_estra,
      wg_estra            TYPE ty_estra,
      tg_estra2           TYPE TABLE OF ty_estra,
      wg_estra2           TYPE ty_estra,
      it_zglt034          TYPE TABLE OF ty_zglt034,
      it_zglt035          TYPE TABLE OF ty_zglt035,
      it_zglt036          TYPE TABLE OF ty_zglt036,
      it_zglt036_ab       TYPE TABLE OF ty_zglt036,
      it_zglt031          TYPE TABLE OF ty_zglt031,
      it_zimp_cad_depto   TYPE TABLE OF zimp_cad_depto,
      it_zib_contabil_chv TYPE TABLE OF ty_zib_contabil_chv,
      it_zglt038          TYPE TABLE OF ty_zglt038,
      tw_zglt038          TYPE TABLE OF ty_zglt038 WITH HEADER LINE,
      tw_zglt038_nivel    TYPE TABLE OF ty_zglt038 WITH HEADER LINE,
      it_faglflexa        TYPE TABLE OF faglflexa,
      wa_faglflexa        TYPE faglflexa,
      it_zgl34_fbb1       TYPE TABLE OF ty_zgl34_fbb1,
      wa_zgl34_fbb1       TYPE ty_zgl34_fbb1,
      it_bkpf             TYPE TABLE OF ty_bkpf,
      it_bsak             TYPE TABLE OF ty_bsak,
      it_tbsl             TYPE TABLE OF ty_tbsl,
      it_zib_contabil     TYPE TABLE OF ty_zib_contabil,
      it_t001             TYPE TABLE OF ty_t001,
      it_t005             TYPE TABLE OF ty_t005,
      t_tcurr             TYPE TABLE OF ty_tcurr,
      it_saida            TYPE TABLE OF ty_saida,
      it_saida_aux        TYPE TABLE OF ty_saida,
      it_color            TYPE TABLE OF lvc_s_scol,
      it_saida_aux2       TYPE TABLE OF ty_saida,
      wa_saida_aux2       TYPE ty_saida.

DATA: wa_zglt035_aux TYPE zglt035,
      it_zglt036_aux TYPE TABLE OF zglt036,
      it_zglt031_aux TYPE TABLE OF zglt031 WITH HEADER LINE.

DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
      wa_zglt036_flg TYPE zde_zglt036_flg.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: ok-code             TYPE sy-ucomm,
      wa_cont             TYPE REF TO cl_gui_custom_container,
      wa_alv              TYPE REF TO cl_gui_alv_grid,
      wa_layout           TYPE lvc_s_layo,
      wa_zglt035          TYPE ty_zglt035,
      wa_zglt036          TYPE ty_zglt036,
      wa_zglt036_ab       TYPE ty_zglt036,
      wa_zglt031          TYPE ty_zglt031,
      wa_zglt034          TYPE ty_zglt034,
      wa_zib_contabil_chv TYPE ty_zib_contabil_chv,
      wa_zglt038          TYPE ty_zglt038,
      wa_bkpf             TYPE ty_bkpf,
      wa_bsak             TYPE ty_bsak,
      wa_tbsl             TYPE ty_tbsl,
      wa_zib_contabil     TYPE ty_zib_contabil,
      wa_t001             TYPE ty_t001,
      wa_t005             TYPE ty_t005,
      wa_tcurr            TYPE ty_tcurr,
      wa_zimp_cad_depto   TYPE zimp_cad_depto,
      wa_saida            TYPE ty_saida,
      wa_saida_aux        TYPE ty_saida,
      wa_color            TYPE lvc_s_scol.
*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  it_fcat       TYPE TABLE OF ty_estrutura,
  s_variant     TYPE disvariant           , " Tabela Estrutura co
  t_top         TYPE slis_t_listheader,
  xs_events     TYPE slis_alv_event,
  events        TYPE slis_t_event,
  gd_layout     TYPE slis_layout_alv,
  t_print       TYPE slis_print_alv,
  v_report      LIKE sy-repid,
  t_sort        TYPE slis_t_sortinfo_alv WITH HEADER LINE,
  it_setleaf    LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
  estrutura     TYPE TABLE OF ty_estrutura,
  vg_i          TYPE i,
  v_repid       LIKE sy-repid,
  v_continua(1),
  v_grava(1),
  vl_lote       TYPE zglt034-lote,
  e_num_lote    TYPE zlote_num,
  e_num_doc	    TYPE num10,
  var_refresh   TYPE i,
  var_corrigi   TYPE i,
  var_reproc    TYPE i,
  v_tcode       TYPE sy-tcode.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: grid2               TYPE REF TO cl_gui_alv_grid,
      obg_conteiner_estra TYPE REF TO cl_gui_custom_container,
      g_cc_estra          TYPE scrfname VALUE 'CC_ESTRA',
      wa_stable           TYPE lvc_s_stbl,
      t_fieldcatalog      TYPE lvc_t_fcat,
      w_fieldcatalog      TYPE lvc_s_fcat.

DATA: v_msg   TYPE char50,
      t_lotes TYPE TABLE OF zfi_lotes_imp,
      w_lotes TYPE          zfi_lotes_imp,
      vg_lote TYPE          zglt034-lote,
      t_estra TYPE TABLE OF zfi_estrategia_zgl,
      w_estra TYPE          zfi_estrategia_zgl,
      t_docs  TYPE TABLE OF zgl_docs_imp,
      w_docs  TYPE          zgl_docs_imp.

DATA: repid            LIKE sy-repid.
DATA: s_fieldcat       TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: s_layout         TYPE slis_layout_alv.
DATA: s_print          TYPE slis_print_alv.
DATA: s_sort           TYPE slis_t_sortinfo_alv WITH HEADER LINE.
DATA: variante         LIKE disvariant.
DATA: def_variante     LIKE disvariant.
DATA: s_selfield       TYPE slis_selfield.
DATA: list_top_of_page TYPE slis_t_listheader.
DATA: wl_disvariant    TYPE disvariant. "USER STORY 163210 - MMSILVA - 15.01.2025
DATA: rs_variant       LIKE disvariant. "USER STORY 163210 - MMSILVA - 15.01.2025

DATA wg_texto TYPE ty_texto.
DATA seq TYPE i.



*----------------------------------------------------------------------*
* Inicio Estrutura dos dados Dinamicos
*----------------------------------------------------------------------*
DATA: wa_fcat_lvc TYPE lvc_s_fcat,
      lt_fcat_lvc TYPE lvc_t_fcat,
      v_camp(7),
      v_text(100),
      t_data      TYPE REF TO data.

FIELD-SYMBOLS: <fs_data>  TYPE ANY TABLE,
               <wa_data>  TYPE any,
               <wa_data2> TYPE any,
               <fs_campo> TYPE any.

*----------------------------------------------------------------------*
* Fim Estrutura dos dados Dinamicos
*----------------------------------------------------------------------*

CONSTANTS c_x               TYPE c VALUE 'X'.

DEFINE mc_preenche_class.
  vg_i = vg_i + 1.
  CLEAR t_sort.
  t_sort-spos      = vg_i.
  t_sort-fieldname = &1.
  t_sort-group     = &2.
  t_sort-up        = &3.
  t_sort-subtot    = &4.
  APPEND t_sort.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
"DATA   DYFIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs  FOR zglt034-bukrs.  "FOR zglt034-bukrs OBLIGATORY.
  PARAMETERS: p_li_sta(1) AS LISTBOX VISIBLE LENGTH 20 DEFAULT '',
              p_li_stc(1) AS LISTBOX VISIBLE LENGTH 20 DEFAULT ''.
  SELECT-OPTIONS: p_lote      FOR  zglt034-lote,
                  p_data      FOR  sy-datum,
                  p_doclct    FOR  zglt036-doc_lcto,
                  p_dept      FOR  zimp_cad_depto-dep_resp,
                  p_usu       FOR  zimp_aprovador-aprovador.
*                P_USU       FOR  ZIMP_APROVADOR-WAERS MATCHCODE OBJECT /BA1/F4_FX_WAERS.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-038.
  PARAMETER: p_ffbi  AS CHECKBOX,
             p_eqdif AS CHECKBOX. "DEVK9A1PN1 - FI - Equivalencia - ZGL020 #123014 - RSA
SELECTION-SCREEN END OF BLOCK b2.

*USER STORY 163210- MMSILVA - 15.01.2025 - Inicio
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-041.
  PARAMETERS: p_layout TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f_alv_variant_f4 CHANGING p_layout.
*USER STORY 163210 - MMSILVA - 15.01.2025 - Fim

AT SELECTION-SCREEN OUTPUT.

**********************************************************************
* Valida Usuário Internacionais
**********************************************************************
  SELECT SINGLE *
      FROM usr05
      INTO @DATA(_usr05)
      WHERE bname = @sy-uname
      AND parid   = 'BUK'.
  IF sy-subrc = 0.
    p_bukrs = _usr05-parva+0(4) .
  ENDIF.

  DATA: name  TYPE vrm_id,
        list  TYPE vrm_values,
        value TYPE vrm_value.

  REFRESH list.
  name = 'P_LI_STA'. " Name should be in UPPER CASE

  value-key = '1'.
  value-text =  TEXT-s01.
  APPEND value TO list.

  value-key = '2'.
  value-text =  TEXT-s02.
  APPEND value TO list.

  value-key = '3'.
  value-text =  TEXT-s03.
  APPEND value TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = name
      values          = list
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.

  REFRESH list.
  name = 'P_LI_STC'. " Name should be in UPPER CASE

  value-key = '1'.
  value-text = TEXT-s04 .
  APPEND value TO list.

  value-key = '2'.
  value-text =  TEXT-s05 .
  APPEND value TO list.


  value-key = '3'.
  value-text =  TEXT-s06.
  APPEND value TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = name
      values          = list
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  v_continua = 'S'.
  CLEAR: vl_lote.
  IF sy-tcode = 'ZGL017'.
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_lote.
  ENDIF.
  IF p_bukrs IS INITIAL .
    MESSAGE 'Informe a Empresa!' TYPE 'I'.
  ELSE.
    PERFORM:
              f_iniciar_variaves, " Cabeçalho
              f_seleciona_dados. " Form seleciona dados

    IF erro_acesso IS INITIAL.
      PERFORM:
      f_saida, " Form de saida
      f_imprime_dados.
    ENDIF.


  ENDIF.

END-OF-SELECTION.




*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

*********************************************************************
* Valida Usuário Objeto 110906 Range Filiais Perfil Acxesso - PSA
*********************************************************************

  DATA _exit TYPE c.
  DATA _acesso(1) VALUE 'S'.

  CLEAR: erro_acesso.

  SELECT  bukrs
          FROM t001 WHERE bukrs IN @p_bukrs
          INTO TABLE @DATA(_bukrs).

  LOOP AT _bukrs INTO DATA(_empresa).
    DATA vobj_key TYPE zib_contabil_err-obj_key.
    AUTHORITY-CHECK OBJECT 'F_SKA1_BUK' ID 'ACTVT' FIELD '03'
                                        ID 'BUKRS' FIELD _empresa.

    IF sy-subrc IS NOT INITIAL.
      erro_acesso = 'X'.
      APPEND VALUE #( acesso = 'N' bukrs = _empresa-bukrs message = 'Sem autorização para esta Empresa - ' && _empresa-bukrs ) TO it_acesso.
    ELSE.
      APPEND VALUE #( acesso = 'S' bukrs = _empresa-bukrs message = 'Sem autorização para esta Empresa - ' && _empresa-bukrs ) TO it_acesso.
    ENDIF.



  ENDLOOP.

  READ TABLE it_acesso INTO DATA(_acesso_negado) WITH KEY acesso = 'N'.

  IF _acesso_negado IS NOT INITIAL.

    SORT it_acesso BY acesso.
    DELETE  it_acesso WHERE acesso = 'S'.
    "Construindo ALV pop-up.
    DATA(tl_fieldcat) = VALUE slis_t_fieldcat_alv(

      "( fieldname = 'BURKS     '        seltext_m = 'Empresa           '  outputlen = '10' )
      ( fieldname = 'MESSAGE   '      seltext_m = 'Menssagem           '  outputlen = '50' )
      ).

    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        i_title     = 'Empresas sem Autorização'
        i_selection = 'X'
        i_tabname   = 'IT_ACESSO'
        i_zebra     = 'X'
        it_fieldcat = tl_fieldcat
        "IMPORTING
        "es_selfield = linha_selecionada
        "e_exit      = _exit
      TABLES
        t_outtab    = it_acesso.

    EXIT.

  ENDIF.


*********************************************************************
* Valida Usuário Objeto 110906 Range Filiais Perfil Acxesso - PSA
*********************************************************************
* Código Anterior
*  DATA vobj_key TYPE zib_contabil_err-obj_key.
*  AUTHORITY-CHECK OBJECT 'F_SKA1_BUK'
*         ID 'ACTVT' FIELD '03'       "display
*         ID 'BUKRS' FIELD p_bukrs.
*  IF sy-subrc <> 0.
*    MESSAGE 'Sem autorização para esta Empresa ' TYPE 'E'.
*    EXIT.
*  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Preparando dados'.


  IF p_ffbi IS NOT INITIAL.

    IF p_eqdif IS INITIAL.
      SELECT  lote descr_lote bukrs usnam status_lote dep_resp tcode data_atual
           FROM zglt034
           INTO TABLE it_zglt034
           WHERE lote       IN p_lote
           AND   bukrs      IN p_bukrs
           AND   usnam      IN p_usu
           AND   dep_resp   IN p_dept
           AND   data_atual IN p_data
           AND   tcode      EQ 'FBB1'.
    ELSE.
      SELECT  lote descr_lote bukrs usnam status_lote dep_resp tcode data_atual
          FROM zglt034
          INTO TABLE it_zglt034
          WHERE lote       IN p_lote
          AND   bukrs      IN p_bukrs
          AND   usnam      IN p_usu
          AND   dep_resp   IN p_dept
          AND   data_atual IN p_data.
    ENDIF.

    MOVE-CORRESPONDING it_zglt034 TO it_zgl34_fbb1.

    IF it_zgl34_fbb1[] IS NOT INITIAL.
      SELECT *
        FROM faglflexa INTO TABLE it_faglflexa
          FOR ALL ENTRIES IN it_zgl34_fbb1
      WHERE  ryear  EQ it_zgl34_fbb1-data_atual+0(4)
        AND  docnr  EQ it_zgl34_fbb1-lote
        AND  rbukrs EQ it_zgl34_fbb1-bukrs
        AND  drcrk  EQ 'S'
        AND  rldnr  EQ '50'.
    ENDIF.



  ELSE.

    IF vl_lote GT 0 AND sy-tcode = 'ZGL017'.
      SELECT  lote descr_lote bukrs usnam status_lote dep_resp tcode data_atual
          FROM zglt034
          INTO TABLE it_zglt034
          WHERE lote  EQ vl_lote
          AND   bukrs IN p_bukrs.

    ELSE.
      SELECT  lote descr_lote bukrs usnam status_lote dep_resp tcode data_atual
        FROM zglt034
        INTO TABLE it_zglt034
        WHERE lote IN p_lote
        AND   bukrs IN p_bukrs
        AND   usnam IN p_usu
        AND   dep_resp IN p_dept
        AND   EXISTS ( SELECT * FROM zglt035
                          WHERE zglt035~bukrs = zglt034~bukrs
                          AND   zglt035~lote  = zglt034~lote
                          AND   zglt035~budat IN p_data
                          AND   zglt035~loekz = ''
                         AND   zglt035~doc_lcto IN p_doclct
                          ) .
    ENDIF.

  ENDIF.

  CHECK it_zglt034[] IS NOT INITIAL.



  SELECT  doc_lcto bukrs tp_lcto lote moeda_doc st_lc_moeda bldat budat dt_lcto taxa belnr dpto_resp
          lote_prec doc_lcto_prec belnr_prec
    FROM zglt035
    INTO TABLE it_zglt035
    FOR ALL ENTRIES IN it_zglt034
    WHERE bukrs = it_zglt034-bukrs
    AND   lote  = it_zglt034-lote
    AND   budat IN p_data
    AND doc_lcto IN p_doclct
    AND   loekz = ''.

  CHECK it_zglt035[] IS NOT INITIAL.

  SELECT *
    FROM zimp_cad_depto
    INTO TABLE it_zimp_cad_depto
    FOR ALL ENTRIES IN it_zglt034
  WHERE dep_resp = it_zglt034-dep_resp.

  LOOP AT it_zglt035 INTO wa_zglt035.

    IF wa_zglt035-belnr IS INITIAL.
      CONCATENATE 'ZGL17' wa_zglt035-doc_lcto '%' INTO vobj_key.

      SELECT SINGLE obj_key belnr bukrs gjahr
      FROM zib_contabil_chv
      INTO wa_zib_contabil_chv
      WHERE obj_key LIKE vobj_key
      AND   bukrs IN p_bukrs.

      IF sy-subrc = 0.
        wa_zglt035-doc_contabil = wa_zib_contabil_chv-belnr.
        wa_zglt035-gjahr        = wa_zib_contabil_chv-gjahr.
        MODIFY it_zglt035 FROM wa_zglt035 INDEX sy-tabix TRANSPORTING doc_contabil gjahr .
        SELECT SINGLE bukrs belnr gjahr budat stblg stjah
          FROM bkpf
          INTO wa_bkpf
          WHERE bukrs = wa_zib_contabil_chv-bukrs
          AND   belnr = wa_zib_contabil_chv-belnr
        AND   gjahr = wa_zib_contabil_chv-gjahr.
        IF sy-subrc = 0.
          wa_zglt035-doc_contabil_e = wa_bkpf-stblg.
          wa_zglt035-gjahr_e        = wa_bkpf-stjah.
          wa_zglt035-budat          = wa_bkpf-budat.
          MODIFY it_zglt035 FROM wa_zglt035 INDEX sy-tabix TRANSPORTING doc_contabil_e gjahr_e  budat.
        ENDIF.
        SELECT SINGLE bukrs belnr gjahr augbl
          FROM bsak
          INTO wa_bsak
          WHERE  bukrs =  wa_zib_contabil_chv-bukrs
          AND    belnr =  wa_zib_contabil_chv-belnr
        AND    gjahr =  wa_zib_contabil_chv-gjahr.
        IF sy-subrc = 0.
          wa_zglt035-augbl = wa_bsak-augbl.
          MODIFY it_zglt035 FROM wa_zglt035 INDEX sy-tabix TRANSPORTING augbl.
        ENDIF.
      ELSE.
        CONCATENATE 'ZGL17' wa_zglt035-doc_lcto '%' INTO vobj_key.
        SELECT SINGLE obj_key
          FROM zib_contabil_err
          INTO wa_zib_contabil_chv
        WHERE obj_key LIKE vobj_key.
        IF sy-subrc = 0.
          wa_zglt035-flg_err = 'S'.
          MODIFY it_zglt035 FROM wa_zglt035 INDEX sy-tabix TRANSPORTING flg_err.
        ENDIF.
      ENDIF.

      "130130 - CS2023000969 Gisele Follmann PSA
      DATA: reversao_obj_key TYPE zib_contabil-obj_key.
      CLEAR: reversao_obj_key.
      reversao_obj_key = |{ vobj_key }R|.

      SELECT SINGLE * FROM zib_contabil_chv WHERE obj_key LIKE @reversao_obj_key INTO @DATA(reversao_zib_contabil_chv).

      IF sy-subrc = 0.
        wa_zglt035-reversao_doc = reversao_zib_contabil_chv-belnr.
        MODIFY it_zglt035 FROM wa_zglt035 INDEX sy-tabix TRANSPORTING reversao_doc.
      ENDIF.

    ELSE.
      wa_zglt035-doc_contabil = wa_zglt035-belnr.
      MODIFY it_zglt035 FROM wa_zglt035 INDEX sy-tabix TRANSPORTING doc_contabil .
      SELECT SINGLE bukrs belnr gjahr budat stblg stjah
        FROM bkpf
        INTO wa_bkpf
        WHERE bukrs = wa_zglt035-bukrs
      AND   belnr = wa_zglt035-belnr.
      IF sy-subrc = 0.
        wa_zglt035-budat = wa_bkpf-budat.
        wa_zglt035-gjahr = wa_bkpf-gjahr.
        wa_zglt035-doc_contabil_e = wa_bkpf-stblg.
        wa_zglt035-gjahr_e        = wa_bkpf-stjah.
        MODIFY it_zglt035 FROM wa_zglt035 INDEX sy-tabix TRANSPORTING doc_contabil_e gjahr_e budat gjahr.
      ENDIF.
      SELECT SINGLE bukrs belnr gjahr augbl
       FROM bsak
       INTO wa_bsak
       WHERE bukrs = wa_zglt035-bukrs
       AND   belnr = wa_zglt035-belnr
      AND   gjahr = wa_zglt035-gjahr.
      IF sy-subrc = 0.
        wa_zglt035-augbl = wa_bsak-augbl.
        MODIFY it_zglt035 FROM wa_zglt035 INDEX sy-tabix TRANSPORTING augbl.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SELECT  doc_lcto seqitem seqsub  tp_lcto  bschl hkont umskz anbwa bewar vbund kostl prctr aufnr matnr zuonr sgtxt gsber
          vlr_moeda_doc vlr_moeda_int vlr_moeda_forte vlr_moeda_grupo
    FROM zglt036
    INTO TABLE it_zglt036
    FOR ALL ENTRIES IN it_zglt035
  WHERE doc_lcto = it_zglt035-doc_lcto.

  SELECT bschl shkzg
  FROM tbsl
  INTO TABLE it_tbsl
  FOR ALL ENTRIES IN it_zglt036
  WHERE bschl EQ it_zglt036-bschl.

  SELECT  tp_lcto descricao
    FROM zglt031
    INTO TABLE it_zglt031
    FOR ALL ENTRIES IN it_zglt035
  WHERE tp_lcto = it_zglt035-tp_lcto.

  SELECT bukrs lote nivel aprovador valor_de valor_ate data_atual hora_atual usuario
    FROM zglt038
    INTO TABLE tw_zglt038
    FOR ALL ENTRIES IN it_zglt035
  WHERE lote EQ it_zglt035-lote.




ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .

  DATA: xtotal TYPE zglt036-vlr_moeda_int,
        tabix1 TYPE sy-tabix.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Gerando relatório'.

  SORT: it_zglt035 BY doc_lcto,
        it_zglt036 BY doc_lcto,
        it_zglt031 BY tp_lcto,
        it_zglt034 BY lote,
        it_tbsl    BY bschl,
        it_zimp_cad_depto BY dep_resp.

  SELECT bukrs butxt land1
   FROM t001
   INTO TABLE it_t001
   FOR ALL ENTRIES IN it_zglt034
  WHERE  bukrs EQ it_zglt034-bukrs.

  SELECT land1 waers
  FROM t005
  INTO TABLE it_t005
  FOR ALL ENTRIES IN it_t001
  WHERE land1 = it_t001-land1.

  SELECT kurst fcurr tcurr gdatu ukurs
    FROM tcurr
    INTO TABLE t_tcurr
    FOR ALL ENTRIES IN it_t005
    WHERE kurst = 'B'
    AND   fcurr EQ 'USD'
  AND   tcurr EQ it_t005-waers.

  CLEAR: v_tcode.

  IF p_ffbi IS NOT INITIAL.

    LOOP AT it_zgl34_fbb1 INTO wa_zgl34_fbb1.
      wa_saida-icon           = icon_checked.
      wa_saida-aprov          = wa_zgl34_fbb1-status_lote.
      wa_saida-doc_contabil   = wa_zgl34_fbb1-lote.
      wa_saida-budat          = wa_zgl34_fbb1-data_atual.
      wa_saida-lote           = wa_zgl34_fbb1-lote.
      wa_saida-descr_lote     = wa_zgl34_fbb1-descr_lote.
      wa_saida-dep_resp_desc  = wa_zgl34_fbb1-dep_resp.
      wa_saida-usnam          = wa_zgl34_fbb1-usuario.
      wa_saida-tcode          = wa_zgl34_fbb1-tcode.

      "BUG SOLTO 103403* / AOENNING / 23-02-2023
      IF p_eqdif EQ abap_true.

        IF wa_saida-tcode NE 'ZGL076' AND
           wa_saida-tcode NE 'ZGL081' AND
           wa_saida-tcode NE 'ZGL082' .
          wa_saida-lote = ''.
          v_tcode = wa_saida-tcode.
          CONTINUE.
        ENDIF.

      ELSE.
        " DEVK9A1PN1 - FI - Equivalencia - ZGL020 #123014 - RSA
        " Opção Equivalência / Diferido - apenas os documentos dessas TCODE
        IF wa_saida-tcode EQ 'ZGL076' OR
           wa_saida-tcode EQ 'ZGL081' OR
           wa_saida-tcode EQ 'ZGL082' .
          wa_saida-lote = ''.
          v_tcode = wa_saida-tcode.
          CONTINUE.
        ENDIF.

      ENDIF.

      wa_saida-bukrs          = wa_zgl34_fbb1-bukrs.

      IF wa_zgl34_fbb1-status_lote = ' '  .
        wa_saida-aprov =  TEXT-s01.
      ELSEIF wa_zgl34_fbb1-status_lote = 'L' .
        wa_saida-aprov =  TEXT-s02.
      ELSEIF wa_zgl34_fbb1-status_lote = 'A' .
        wa_saida-aprov =  TEXT-s03.
      ENDIF.

      SELECT  SINGLE bukrs
                     belnr
                     gjahr
                     budat
                     stblg
                     stjah
        FROM bkpf INTO wa_bkpf
        WHERE bukrs  EQ wa_zgl34_fbb1-bukrs
         AND  stblg  EQ wa_zgl34_fbb1-lote.

      IF sy-subrc = 0.
        wa_saida-doc_contabil_e =  wa_bkpf-belnr.
      ENDIF.

      wa_saida-gjahr     =  wa_zgl34_fbb1-data_atual+0(4).


      LOOP AT it_faglflexa INTO wa_faglflexa WHERE docnr = wa_zgl34_fbb1-lote.
        ADD  wa_faglflexa-wsl TO wa_saida-vlr_moeda_doc.
        ADD  wa_faglflexa-osl TO wa_saida-vlr_moeda_forte.
        ADD  wa_faglflexa-hsl TO wa_saida-vlr_moeda_int.
      ENDLOOP.

      APPEND wa_saida TO it_saida.
      CLEAR: wa_saida, wa_faglflexa, wa_bkpf,wa_zgl34_fbb1.
    ENDLOOP.

  ELSE.

    SORT: it_t001             BY bukrs,
          it_t005             BY land1,
          t_tcurr             BY tcurr gdatu.

    LOOP AT it_zglt035 INTO wa_zglt035.
      READ TABLE it_zglt034 INTO wa_zglt034 WITH KEY lote = wa_zglt035-lote BINARY SEARCH.

      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zglt034-bukrs BINARY SEARCH.
      CLEAR wa_tcurr .
      READ TABLE it_t005 INTO wa_t005 WITH KEY land1 =  wa_t001-land1 BINARY SEARCH.
      LOOP AT t_tcurr INTO wa_tcurr WHERE tcurr = wa_t005-waers.
        EXIT.
      ENDLOOP.

      wa_saida-tcode = wa_zglt034-tcode.
      wa_saida-usnam = wa_zglt034-usnam.
      IF wa_zglt034-status_lote = ' '  .
        wa_saida-aprov =  TEXT-s01.
      ELSEIF wa_zglt034-status_lote = 'L' .
        wa_saida-aprov =  TEXT-s02.
      ELSEIF wa_zglt034-status_lote = 'A' .
        wa_saida-aprov =  TEXT-s03.
      ENDIF.

      IF p_li_sta = 1.
        IF wa_zglt034-status_lote NE ' '.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF p_li_sta = 2.
        IF wa_zglt034-status_lote NE 'L'.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF p_li_sta = 3.
        IF wa_zglt034-status_lote NE 'A'.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF wa_zglt035-doc_contabil IS NOT INITIAL.
        wa_saida-icon = icon_checked.
      ELSEIF wa_zglt035-flg_err = 'S'.
        wa_saida-icon = icon_message_error.
      ELSE.
        wa_saida-icon = icon_led_yellow.
      ENDIF.

      IF p_li_stc = 1.
        IF wa_saida-icon NE icon_checked.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF p_li_stc = 2.
        IF wa_saida-icon NE icon_message_error.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF p_li_stc = 3.
        IF wa_saida-icon NE icon_led_yellow.
          CONTINUE.
        ENDIF.
      ENDIF.

      wa_saida-lote           = wa_zglt035-lote.
      wa_saida-doc_contabil   = wa_zglt035-doc_contabil.
      wa_saida-doc_contabil_e = wa_zglt035-doc_contabil_e.
      wa_saida-reversao_doc     = wa_zglt035-reversao_doc. "130130 - CS2023000969 Gisele Follmann PSA
      wa_saida-augbl          = wa_zglt035-augbl.
      wa_saida-bukrs          = wa_zglt035-bukrs.
      wa_saida-gjahr          = wa_zglt035-gjahr.
      wa_saida-gjahr_e        = wa_zglt035-gjahr_e.
      wa_saida-doc_lcto       = wa_zglt035-doc_lcto.
      wa_saida-tp_lcto        = wa_zglt035-tp_lcto.
      wa_saida-budat          = wa_zglt035-budat.
      wa_saida-descr_lote     = wa_zglt034-descr_lote.


      READ TABLE it_zglt031 INTO wa_zglt031 WITH KEY tp_lcto = wa_zglt035-tp_lcto BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-descricao = wa_zglt031-descricao.
      ENDIF.
      wa_saida-vlr_moeda_doc  = 0.
      wa_saida-vlr_moeda_int  = 0.
      wa_saida-vlr_moeda_forte  = 0.
      wa_saida-vlr_moeda_grupo   = 0.
      wa_saida-vlr_total = 0.

      LOOP AT it_zglt036 INTO wa_zglt036 WHERE doc_lcto = wa_zglt035-doc_lcto.
        READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_zglt036-bschl BINARY SEARCH.
        IF wa_tbsl-shkzg NE 'H'.
          ADD wa_zglt036-vlr_moeda_doc    TO wa_saida-vlr_moeda_doc.
          ADD wa_zglt036-vlr_moeda_int    TO wa_saida-vlr_moeda_int.
          ADD wa_zglt036-vlr_moeda_forte  TO wa_saida-vlr_moeda_forte.
          ADD wa_zglt036-vlr_moeda_grupo  TO wa_saida-vlr_moeda_grupo.
          IF wa_zglt035-st_lc_moeda = 'X'.
            ADD wa_zglt036-vlr_moeda_doc TO wa_saida-vlr_total.
          ELSEIF wa_zglt035-moeda_doc = wa_t005-waers.
            ADD wa_zglt036-vlr_moeda_int  TO wa_saida-vlr_total.
            wa_tcurr-ukurs = 1.
          ELSEIF wa_zglt035-moeda_doc = 'USD'.
            ADD wa_zglt036-vlr_moeda_forte  TO wa_saida-vlr_total.
          ENDIF.

          xtotal = 0.
          IF wa_zglt036-vlr_moeda_int GT 0.
            ADD wa_zglt036-vlr_moeda_int TO xtotal.
          ELSEIF wa_zglt035-st_lc_moeda = 'X'.
            IF  wa_zglt036-vlr_moeda_doc GT 0.
              xtotal = xtotal + (  wa_zglt036-vlr_moeda_doc * wa_tcurr-ukurs ).
            ELSE.
              xtotal = xtotal + (  wa_zglt036-vlr_moeda_forte * wa_tcurr-ukurs ).
            ENDIF.
          ELSEIF wa_zglt036-vlr_moeda_forte GT 0.
            xtotal = (  wa_zglt036-vlr_moeda_forte * wa_tcurr-ukurs ).
          ELSE.
            xtotal = xtotal + (  wa_zglt036-vlr_moeda_doc * wa_tcurr-ukurs ).
          ENDIF.
          ADD xtotal TO wa_saida-vlr_total_lote.
        ENDIF.
      ENDLOOP.

      READ TABLE it_zimp_cad_depto INTO wa_zimp_cad_depto WITH KEY dep_resp = wa_zglt034-dep_resp BINARY SEARCH.
      CONCATENATE  wa_zimp_cad_depto-dep_resp '-' wa_zimp_cad_depto-dep_resp_desc INTO wa_saida-dep_resp_desc.
      wa_saida-dep_resp = wa_zglt034-dep_resp.

      wa_saida-lote_prec      = wa_zglt035-lote_prec.
      wa_saida-doc_lcto_prec  = wa_zglt035-doc_lcto_prec.
      wa_saida-belnr_prec     = wa_zglt035-belnr_prec.

      "BUG SOLTO 103403* / AOENNING / 23-02-2023
      IF p_eqdif EQ abap_true.

        IF wa_saida-tcode NE 'ZGL076' AND
           wa_saida-tcode NE 'ZGL081' AND
           wa_saida-tcode NE 'ZGL082' .
          wa_saida-lote = ''.
          v_tcode = wa_saida-tcode.
          CONTINUE.
        ENDIF.

      ELSE.

        " DEVK9A1PN1 - FI - Equivalencia - ZGL020 #123014 - RSA
        " Opção Equivalência / Diferido - apenas os documentos dessas TCODE
        IF wa_saida-tcode EQ 'ZGL076' OR
           wa_saida-tcode EQ 'ZGL081' OR
           wa_saida-tcode EQ 'ZGL082' .
          wa_saida-lote = ''.
          v_tcode = wa_saida-tcode.
          CONTINUE.
        ENDIF.

      ENDIF.

      APPEND wa_saida TO  it_saida.
      CLEAR: wa_saida.

    ENDLOOP.
    it_saida_aux[] = it_saida[].
    SORT it_saida_aux BY lote.
    DELETE ADJACENT DUPLICATES FROM it_saida_aux COMPARING lote.
    LOOP AT it_saida_aux INTO wa_saida_aux.
      wa_saida_aux-vlr_total_lote = 0.
      tabix1 = sy-tabix.
      LOOP AT it_saida INTO wa_saida WHERE lote = wa_saida_aux-lote.
        ADD wa_saida-vlr_total_lote TO wa_saida_aux-vlr_total_lote.
      ENDLOOP.
      MODIFY it_saida_aux FROM wa_saida_aux INDEX tabix1 TRANSPORTING vlr_total_lote.
    ENDLOOP.

    LOOP AT it_saida INTO wa_saida.
      tabix1 = sy-tabix.
      READ TABLE it_saida_aux INTO wa_saida_aux WITH KEY lote = wa_saida-lote BINARY SEARCH.
      wa_saida-vlr_total_lote = wa_saida_aux-vlr_total_lote.
      MODIFY it_saida FROM wa_saida INDEX tabix1 TRANSPORTING vlr_total_lote.
    ENDLOOP.
    REFRESH it_saida_aux .

  ENDIF.
ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .
  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_alv.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                     slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
FORM top_of_page.

* Cabeçalho Logo
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = list_top_of_page[].
  "I_LOGO             = 'WELLA_LOGO'.

ENDFORM.        " top_of_page.

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort .

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv .

  FREE: it_fcat, lt_fcat_lvc.

  seq = 0.
  PERFORM alv_preenche_cat USING:
               'ICON'             TEXT-026   '10'  ' '  ' '  ' ' , "Icon
               'APROV'            TEXT-027   '20'  ' '  ' '  ' ' , "Aprovação
               'DOC_CONTABIL'     TEXT-014   '15'  'X'  ' '  ' ' , "Doc.contábil.
               'DOC_CONTABIL_E'   TEXT-028   '15'  'X'  ' '  ' ' , "Doc.contábil Estorno.
               'REVERSAO_DOC'     TEXT-040   '15'  'X'  ' '  ' ' , "Doc. Reversão "130130 - CS2023000969 Gisele Follmann PSA
               'DEP_RESP_DESC'    TEXT-023   '20'  ' '  ' '  ' ' . "Departamento.

  "BUG SOLTO 103403* / AOENNING / 23-02-2023
*  IF v_tcode IS INITIAL.
  PERFORM alv_preenche_cat USING:
    'LOTE'             TEXT-015   '10'  ' '  ' '  ' ' . "Lote
*  ENDIF.
  PERFORM alv_preenche_cat USING:
 'BUKRS'            TEXT-039   '15'  ' '  ' '  ' ' , "Empresa. 109746 - CS2023000288 ZGL - Seleção Multipla - PSA
 'DOC_LCTO'         TEXT-002   '15'  'X'  ' '  ' ' , "Doc.Imp.
 'TP_LCTO'          TEXT-005   '10'  ' '  ' '  ' ' , "Cod.Imp.
 'DESCRICAO'        TEXT-004   '20'  ' '  ' '  ' ' , "Descr.Imposto
 'BUDAT'            TEXT-016   '15'  ' '  ' '  ' ' , "Dt.Lançamento
 'VLR_MOEDA_DOC'    TEXT-025   '15'  ' '  ' '  ' ' ,
 'VLR_MOEDA_INT'    TEXT-013   '15'  ' '  ' '  ' ' ,
 'VLR_MOEDA_FORTE'  TEXT-020   '15'  ' '  ' '  ' ' ,
 'VLR_MOEDA_GRUPO'  TEXT-021   '15'  ' '  ' '  ' ' ,
 'DESCR_LOTE'       TEXT-022   '30'  ' '  ' '  ' ' ,
 'USNAM'            TEXT-024   '15'  ' '  ' '  ' ' ,
 'LOTE_PREC'        TEXT-029   '15'  ' '  ' '  ' ' ,
 'DOC_LCTO_PREC'    TEXT-030   '20'  'X'  ' '  ' ' ,
 'BELNR_PREC'       TEXT-031   '20'  'X'  ' '  ' ' ,
 'TCODE'            TEXT-037   '20'  ' '  ' '  ' ' .

  seq = 0.
  PERFORM monta_fieldcat USING:
               'ICON'             TEXT-026   '10' ''                  '',
               'APROV'            TEXT-027   '20' ''                  '',
               'DOC_CONTABIL'     TEXT-014   '15' 'ZIB_CONTABIL_CHV'  'BELNR',
               'DOC_CONTABIL_E'   TEXT-028   '15' 'ZIB_CONTABIL_CHV'  'BELNR',
               'REVERSAO_DOC'     TEXT-040   '15' 'ZIB_CONTABIL_CHV'  'BELNR', "Doc. Reversão "130130 - CS2023000969 Gisele Follmann PSA
               'DEP_RESP_DESC'    TEXT-023   '20' 'ZIMP_CAD_DEPTO'    'DEP_RESP_DESC',
               'LOTE'             TEXT-015   '10' 'ZGLT035'           'LOTE',
               'BUKRS'            TEXT-039   '15' 'ZGLT034'           'BUKRS', "Empresa. 109746 - CS2023000288 ZGL - Seleção Multipla - PSA
               'DOC_LCTO'         TEXT-002   '15' 'ZGLT035'           'DOC_LCTO',
               'TP_LCTO'          TEXT-005   '10' 'ZGLT035'           'TP_LCTO',
               'DESCRICAO'        TEXT-004   '20' 'ZGLT031'           'DESCRICAO',
               'BUDAT'            TEXT-016   '15' 'BKPF'              'BUDAT',
               'VLR_MOEDA_DOC'    TEXT-025   '15' 'ZGLT036'           'VLR_MOEDA_DOC',
               'VLR_MOEDA_INT'    TEXT-013   '15' 'ZGLT036'           'VLR_MOEDA_INT',
               'VLR_MOEDA_FORTE'  TEXT-020   '15' 'ZGLT036'           'VLR_MOEDA_FORTE',
               'VLR_MOEDA_GRUPO'  TEXT-021   '15' 'ZGLT036'           'VLR_MOEDA_GRUPO',
               'DESCR_LOTE'       TEXT-022   '30' 'ZGLT034'           'DESCR_LOTE',
               'USNAM'            TEXT-024   '15' 'ZGLT034'           'USNAM',
               'LOTE_PREC'        TEXT-029   '15' 'ZGLT035'           'LOTE_PREC',
               'DOC_LCTO_PREC'    TEXT-030   '20' 'ZGLT035'           'DOC_LCTO_PREC',
               'BELNR_PREC'       TEXT-031   '20' 'ZGLT035'           'BELNR_PREC',
               'TCODE'            TEXT-037   '20' 'ZGLT034'           'TCODE'.



  tw_zglt038_nivel[] = tw_zglt038[].

  SORT tw_zglt038_nivel BY nivel.

  DELETE ADJACENT DUPLICATES FROM tw_zglt038_nivel COMPARING nivel.

  LOOP AT tw_zglt038_nivel.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = tw_zglt038_nivel-nivel
      IMPORTING
        output = tw_zglt038_nivel-nivel.

    CLEAR v_camp.
    v_camp = tw_zglt038_nivel-nivel.
    v_camp = |APROV{ v_camp }|.

    CLEAR v_text.
    v_text = tw_zglt038_nivel-nivel.
    v_text = |{ v_text }º Aprovador|.

    PERFORM monta_fieldcat   USING  v_camp v_text '15' 'ZGLT038' 'APROVADOR'.
    PERFORM alv_preenche_cat USING: v_camp v_text '15' '' '' ''.

    CLEAR v_camp.
    v_camp = tw_zglt038_nivel-nivel.
    v_camp = |DATA{ v_camp }|.

    CLEAR v_text.
    v_text = tw_zglt038_nivel-nivel.
    v_text = |Data { v_text }º Aprovação |.

    PERFORM monta_fieldcat   USING  v_camp v_text '10' 'ZGLT038' 'DATA_ATUAL'.
    PERFORM alv_preenche_cat USING: v_camp v_text '10' '' '' ''.

    CLEAR v_camp.
    v_camp = tw_zglt038_nivel-nivel.
    v_camp = |HORA{ v_camp }|.

    CLEAR v_text.
    v_text = tw_zglt038_nivel-nivel.
    v_text = |Hora { v_text }º Aprovação |.

    PERFORM monta_fieldcat   USING  v_camp v_text '10' 'ZGLT038' 'HORA_ATUAL'.
    PERFORM alv_preenche_cat USING: v_camp v_text '10' '' '' ''.


  ENDLOOP.

  DATA: t_alvdata TYPE REF TO data.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      i_style_table   = ' '
      it_fieldcatalog = lt_fcat_lvc
    IMPORTING
      ep_table        = t_data.

  IF <fs_data> IS ASSIGNED.
    UNASSIGN <fs_data>.
    UNASSIGN <wa_data>.
    UNASSIGN <fs_campo> .
  ENDIF.

  ASSIGN t_data->* TO <fs_data>.
  CREATE DATA t_alvdata LIKE LINE OF <fs_data>.
  ASSIGN t_alvdata->* TO <wa_data>.

  REFRESH <fs_data>.

  LOOP AT it_saida INTO wa_saida.



    PERFORM f_carrega_dados USING:
            wa_saida-icon             'ICON            ', "'ICON'
            wa_saida-aprov            'APROV           ', "'APROV'
            wa_saida-doc_contabil     'DOC_CONTABIL    ', "'DOC_CONTABIL'
            wa_saida-doc_contabil_e   'DOC_CONTABIL_E  ', "'DOC_CONTABIL_E'
            wa_saida-reversao_doc     'REVERSAO_DOC    ', "'REVERSAO_DOC' "Doc. Reversão "130130 - CS2023000969 Gisele Follmann PSA
            wa_saida-dep_resp_desc    'DEP_RESP_DESC   ', "'DEP_RESP_DESC'
            wa_saida-lote             'LOTE            ', "'LOTE'
            wa_saida-bukrs            'BUKRS           ', "'EMPRESA'
            wa_saida-doc_lcto         'DOC_LCTO        ', "'DOC_LCTO'
            wa_saida-tp_lcto          'TP_LCTO         ', "'TP_LCTO'
            wa_saida-descricao        'DESCRICAO       ', "'DESCRICAO'
            wa_saida-budat            'BUDAT           ', "'BUDAT'
            wa_saida-vlr_moeda_doc    'VLR_MOEDA_DOC   ', "'VLR_MOEDA_DOC'
            wa_saida-vlr_moeda_int    'VLR_MOEDA_INT   ', "'VLR_MOEDA_INT'
            wa_saida-vlr_moeda_forte  'VLR_MOEDA_FORTE ', "'VLR_MOEDA_FORTE'
            wa_saida-vlr_moeda_grupo  'VLR_MOEDA_GRUPO ', "'VLR_MOEDA_GRUPO'
            wa_saida-descr_lote       'DESCR_LOTE      ', "'DESCR_LOTE'
            wa_saida-usnam            'USNAM           ', "'USNAM'
            wa_saida-lote_prec        'LOTE_PREC       ', "LOTE_PREC    '
            wa_saida-doc_lcto_prec    'DOC_LCTO_PREC   ', "DOC_LCTO_PREC'
            wa_saida-belnr_prec       'BELNR_PREC      ', "BELNR_PREC    '
            wa_saida-tcode            'TCODE           '. "TCODE

    LOOP AT tw_zglt038 WHERE lote EQ wa_saida-lote.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = tw_zglt038-nivel
        IMPORTING
          output = tw_zglt038-nivel.

      CLEAR: v_camp.
      v_camp = tw_zglt038-nivel.
      v_camp = |APROV{ v_camp }|.

      PERFORM f_carrega_dados USING:  tw_zglt038-aprovador  v_camp.

      CLEAR: v_camp.
      v_camp = tw_zglt038-nivel.
      v_camp = |DATA{ v_camp }|.

      PERFORM f_carrega_dados USING:  tw_zglt038-data_atual  v_camp.

      CLEAR: v_camp.
      v_camp = tw_zglt038-nivel.
      v_camp = |HORA{ v_camp }|.

      PERFORM f_carrega_dados USING:  tw_zglt038-hora_atual  v_camp.

    ENDLOOP.

    PERFORM f_carrega_alv USING <fs_data> <wa_data>.
    CLEAR: <wa_data>.

  ENDLOOP.

  PERFORM f_imprime_dados_alv USING <fs_data>.

*  CHECK SY-UCOMM NE 'ATUALIZAR'.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      I_CALLBACK_PROGRAM       = V_REPORT
*      IS_LAYOUT                = GD_LAYOUT
*      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
*      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*      IT_FIELDCAT              = IT_FCAT[]
*      IT_SORT                  = T_SORT[]
*      I_SAVE                   = 'X'
*      IT_EVENTS                = EVENTS
*      IS_PRINT                 = T_PRINT
*    TABLES
*      T_OUTTAB                 = IT_SAIDA.

ENDFORM.

FORM f_imprime_dados_alv USING p_itab_output TYPE table.

  wl_disvariant-variant = p_layout. "USER STORY 163210 - MMSILVA - 15.01.2025

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_report
      is_variant               = wl_disvariant
      is_layout                = gd_layout
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat              = it_fcat
*     it_sort                  = t_sort[]
      i_default                = 'X'
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
    TABLES
      t_outtab                 = p_itab_output.

ENDFORM.

FORM f_carrega_alv USING    p_tab TYPE table p_wa.
  APPEND p_wa TO p_tab.
ENDFORM.

FORM f_carrega_dados USING p_valor p_campo.
  ASSIGN COMPONENT p_campo  OF STRUCTURE <wa_data> TO <fs_campo>.
  MOVE p_valor TO <fs_campo>.
ENDFORM.

FORM monta_fieldcat USING p_field
                          p_text
                          p_out
                          p_tabref
                          p_ref_field.

  CLEAR: wa_fcat_lvc.

  wa_fcat_lvc-fieldname   = p_field.
  wa_fcat_lvc-tabname     = '<FS_DATA>'.
  wa_fcat_lvc-ref_table   = p_tabref.
  wa_fcat_lvc-seltext     = p_text.
  wa_fcat_lvc-outputlen   = p_out.
  wa_fcat_lvc-ref_field   = p_ref_field.

  seq = seq + 1.
  wa_fcat_lvc-col_pos    = seq.

  APPEND wa_fcat_lvc TO lt_fcat_lvc.

ENDFORM.                    " monta_fieldcat


*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.        "#EC CALLED
  DESCRIBE TABLE rt_extab. "Avoid Extended Check Warning
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "Set_pf_status

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*

FORM user_command USING r_ucomm     LIKE sy-ucomm           "#EC CALLED
                        rs_selfield TYPE slis_selfield.


  DATA: v_cont    TYPE i,
        tabix     TYPE sy-tabix,
        vlines1   TYPE i,
        vlines2   TYPE i,
        vdifer(1),
        v_campo   TYPE zglt035-doc_lcto.


  CLEAR v_cont .
  READ TABLE it_saida INTO wa_saida INDEX rs_selfield-tabindex.

  IF r_ucomm EQ '&CEST'. "gera estratégia
    SELECT SINGLE *
      FROM zglt034
      INTO @DATA(_zglt034)
      WHERE lote = @wa_saida-lote.

    IF _zglt034-status_lote = 'A'.
      MESSAGE TEXT-e01 TYPE 'I'.
      EXIT.
    ENDIF.

    v_grava = 'X'.
    REFRESH: t_lotes, t_estra, t_docs.
    CALL FUNCTION 'Z_GL_ESTRATEGIA_LISTA'
      EXPORTING
        v_usuario = sy-uname
        v_lote    = wa_saida-lote
      IMPORTING
        msg       = v_msg
      TABLES
        t_lotes   = t_lotes
        t_estra   = t_estra
        t_docs    = t_docs.

    REFRESH tg_estra.
    LOOP AT t_estra INTO w_estra.
      MOVE-CORRESPONDING w_estra TO wg_estra.
      APPEND wg_estra TO tg_estra.
    ENDLOOP.

    REFRESH tg_estra2.
    SELECT *
     FROM zglt090
     INTO CORRESPONDING FIELDS OF TABLE tg_estra2
     WHERE lote = wa_saida-lote.

    CLEAR vdifer.
    vlines1 = lines( tg_estra ).
    vlines2 = lines( tg_estra2 ).
    IF vlines1 = vlines2.
      LOOP AT tg_estra INTO wg_estra.
        READ TABLE tg_estra2 INTO wg_estra2 WITH KEY nivel = wg_estra-nivel.
        IF sy-subrc = 0.
          IF wg_estra-valor_de  NE wg_estra2-valor_de OR
             wg_estra-valor_ate NE wg_estra2-valor_ate OR
             wg_estra-aprovador NE wg_estra2-aprovador OR
             wg_estra-waers     NE wg_estra2-waers.
            vdifer = 'X'.
            EXIT.
          ENDIF.
        ELSE.
          vdifer = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
      "
      IF vdifer IS INITIAL.
        MESSAGE TEXT-e02 TYPE 'I'.
        EXIT.
      ENDIF.

    ELSE.
      vdifer = 'X'.
    ENDIF.

    CALL SCREEN 0100 STARTING AT 050 3
                     ENDING   AT 165 12.

  ELSEIF r_ucomm EQ '&EST'.

    READ TABLE it_saida INTO wa_saida INDEX rs_selfield-tabindex.

    REFRESH tg_estra.
    SELECT *
     FROM zglt090
     INTO CORRESPONDING FIELDS OF TABLE tg_estra
     WHERE lote = wa_saida-lote.

    REFRESH: t_lotes, t_estra, t_docs.
    CALL FUNCTION 'Z_GL_ESTRATEGIA_LISTA'
      EXPORTING
        v_usuario = sy-uname
        v_lote    = wa_saida-lote
      IMPORTING
        msg       = v_msg
      TABLES
        t_lotes   = t_lotes
        t_estra   = t_estra
        t_docs    = t_docs.

    LOOP AT tg_estra INTO wg_estra.
      tabix = sy-tabix.
      READ TABLE t_estra INTO w_estra WITH KEY nivel = wg_estra-nivel.
      IF sy-subrc = 0.
        wg_estra-estado = w_estra-estado.
        wg_estra-opcoes = w_estra-opcoes.
        MODIFY tg_estra FROM wg_estra INDEX tabix TRANSPORTING estado opcoes.
      ENDIF.

    ENDLOOP.
    CLEAR v_grava.
    CALL SCREEN 0100 STARTING AT 050 3
                     ENDING   AT 165 12.

  ELSEIF r_ucomm EQ 'ATUALIZAR'.

    ADD 1 TO var_refresh.

    REFRESH it_saida.
    PERFORM: f_seleciona_dados, " Form seleciona dados
             f_saida " Form de saida
.
    PERFORM f_imprime_dados.

    rs_selfield-refresh = 'X'.

    CASE sy-ucomm.
      WHEN '&F03' OR '&F15' OR '&F12'.
      WHEN OTHERS.
        EXIT.
    ENDCASE.

    DO.
      IF var_refresh LE 0.
        EXIT.
      ENDIF.
      LEAVE TO SCREEN 0.
      SUBTRACT 1 FROM var_refresh.
    ENDDO.

  ELSEIF r_ucomm EQ '&LOG'.

    DATA: BEGIN OF itab OCCURS 0,
            name(80) TYPE c,
          END OF itab.
    DATA: vdata(10),vhora(10),v_de(15),v_ate(15), msg_lote TYPE char80.

*    PERFORM row_selection.

*    CHECK it_sel_rows IS NOT INITIAL.
*    SORT it_saida ASCENDING BY doc_lcto.

*    READ TABLE it_saida INTO wa_saida INDEX rs_selfield-tabindex.
    IF <fs_data> IS ASSIGNED.
      FREE: it_saida.
      MOVE-CORRESPONDING <fs_data> TO it_saida.
    ENDIF.
    READ TABLE it_saida INTO wa_saida INDEX rs_selfield-tabindex.

    SELECT bukrs lote nivel aprovador valor_de valor_ate data_atual hora_atual usuario
       FROM zglt038
       INTO TABLE it_zglt038
    WHERE  lote  =  wa_saida-lote.
    SORT it_zglt038 BY nivel.
*    ITAB-NAME    = 'NIVEL|APROVADOR    |DATA       |HORA'.
    itab-name+00(05) = TEXT-p02.
    itab-name+05(01) = '|'.
    itab-name+06(12) = TEXT-p03.
    itab-name+19(01) = '|'.
    itab-name+20(10) = TEXT-p04.
    itab-name+31(01) = '|'.
    itab-name+32(10) = TEXT-p05.
    itab-name+43(01) = '|'.
    itab-name+44(15) = TEXT-a01.
    itab-name+60(01) = '|'.
    itab-name+62(15) = TEXT-a02.

    APPEND itab .
    CLEAR itab.
    LOOP AT it_zglt038 INTO wa_zglt038.
      CONCATENATE wa_zglt038-hora_atual+0(2) wa_zglt038-hora_atual+2(2) wa_zglt038-hora_atual+4(2) INTO vhora SEPARATED BY ':'.
      CONCATENATE wa_zglt038-data_atual+6(2) wa_zglt038-data_atual+4(2) wa_zglt038-data_atual+0(4) INTO vdata SEPARATED BY '.'.
      v_de  = wa_zglt038-valor_de.
      v_ate = wa_zglt038-valor_ate.

      itab-name+00(04) = wa_zglt038-nivel.
      itab-name+05(01) = '|'.
      itab-name+06(12) = wa_zglt038-aprovador.
      itab-name+19(01) = '|'.
      itab-name+20(10) = vdata.
      itab-name+31(01) = '|'.
      itab-name+32(10) = vhora.
      itab-name+43(01) = '|'.
      itab-name+44(15) = v_de.
      itab-name+60(01) = '|'.
      itab-name+62(15) = v_ate.
      APPEND itab .
      CLEAR itab.
    ENDLOOP.
    CONCATENATE 'Lote ' wa_saida-lote INTO msg_lote SEPARATED BY space.
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 140
        endpos_row   = 20
        startpos_col = 60
        startpos_row = 15
        titletext    = msg_lote
      TABLES
        valuetab     = itab
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
*    ENDLOOP.
  ELSEIF r_ucomm EQ '&CORRIGIR'.

    ADD 1 TO var_corrigi.

    IF wa_saida-icon = icon_message_error.
      CONCATENATE 'ZGL17' wa_saida-doc_lcto wa_saida-budat+0(4) INTO wa_zib_contabil-obj_key.
      DELETE FROM zib_contabil_err WHERE obj_key = wa_zib_contabil-obj_key.
      DELETE FROM zib_contabil     WHERE obj_key = wa_zib_contabil-obj_key.

      SELECT  COUNT(*)
        INTO v_cont
        FROM zglt035
      WHERE lote = wa_saida-lote.

      IF  v_cont = 1.
        DELETE FROM zglt038  WHERE lote =  wa_saida-lote.
        "Volta Lote para alteração
        UPDATE zglt034 SET status_lote = ''
        WHERE lote = wa_saida-lote.

        wa_saida-icon = icon_led_yellow.
        MODIFY it_saida FROM wa_saida INDEX rs_selfield-tabindex TRANSPORTING icon.

        rs_selfield-refresh = 'X'.

        PERFORM f_imprime_dados.

      ELSE.
        MESSAGE TEXT-001 TYPE 'I'.
        SELECT SINGLE *
          FROM zglt035
          INTO wa_zglt035_aux
        WHERE doc_lcto = wa_saida-doc_lcto.

        CHECK sy-subrc = 0.

        SELECT *
         FROM zglt036
         INTO TABLE it_zglt036_aux
        WHERE doc_lcto = wa_saida-doc_lcto.

        SELECT SINGLE lote descr_lote bukrs usnam status_lote dep_resp
        FROM zglt034
        INTO  wa_zglt034
        WHERE lote  EQ wa_saida-lote
        AND   bukrs EQ wa_saida-bukrs.

        CALL METHOD zcl_gerar_lote=>create_lote
          EXPORTING
            i_bukrs       = wa_zglt034-bukrs
            i_descr_lote  = wa_zglt034-descr_lote
            i_dep_resp    = wa_zglt034-dep_resp
            i_user_resp   = sy-uname
            i_status_lote = ''
          IMPORTING
            e_num_lote    = e_num_lote.

        wa_zglt035_aux-lote    = e_num_lote.
        wa_zglt035_aux-dt_lcto = wa_zglt035_aux-budat.

        CALL METHOD zcl_gerar_lote=>contabilizar_lote
          EXPORTING
            i_zglt036_flg = it_zglt036_flg
          IMPORTING
            e_num_doc     = e_num_doc
          CHANGING
            i_zglt036     = it_zglt036_aux
            i_zglt035     = wa_zglt035_aux.

        CONCATENATE TEXT-015 e_num_lote INTO  v_msg SEPARATED BY space.
        "
        DELETE FROM zglt035 WHERE  doc_lcto = wa_saida-doc_lcto.
        DELETE FROM zglt036 WHERE  doc_lcto = wa_saida-doc_lcto.

        DELETE  it_saida INDEX rs_selfield-tabindex.
        MESSAGE v_msg  TYPE 'I'.

        rs_selfield-refresh = 'X'.

        PERFORM f_imprime_dados.
      ENDIF.
    ELSE.
      MESSAGE TEXT-m01 TYPE 'I'.
    ENDIF.

    CASE sy-ucomm.
      WHEN '&F03' OR '&F15' OR '&F12'.
      WHEN OTHERS.
        EXIT.
    ENDCASE.

    DO.
      IF var_corrigi LE 0.
        EXIT.
      ENDIF.
      LEAVE TO SCREEN 0.
      SUBTRACT 1 FROM var_corrigi.
    ENDDO.



  ELSEIF r_ucomm EQ '&REPROC'.

    IF wa_saida-icon = icon_message_error.

      ADD 1 TO var_reproc.

      CONCATENATE 'ZGL17' wa_saida-doc_lcto wa_saida-budat+0(4) INTO wa_zib_contabil-obj_key.
      DELETE FROM zib_contabil_err WHERE obj_key = wa_zib_contabil-obj_key.
      DELETE FROM zib_contabil     WHERE obj_key = wa_zib_contabil-obj_key.

      SUBMIT z_grava_zib_zgl  WITH p_lote = wa_saida-lote AND RETURN.

      wa_saida-icon = icon_led_yellow.
      MODIFY it_saida FROM wa_saida INDEX rs_selfield-tabindex TRANSPORTING icon.

      rs_selfield-refresh = 'X'.

      PERFORM f_imprime_dados.

      CASE sy-ucomm.
        WHEN '&F03' OR '&F15' OR '&F12'.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

      DO.
        IF var_reproc LE 0.
          EXIT.
        ENDIF.
        LEAVE TO SCREEN 0.
        SUBTRACT 1 FROM var_reproc.
      ENDDO.


    ELSE.
      MESSAGE TEXT-m01 TYPE 'I'.
    ENDIF.

  ELSE.
    v_campo = rs_selfield-value.

    CASE rs_selfield-fieldname.
      WHEN 'DOC_CONTABIL'.
        READ TABLE it_saida INTO wa_saida WITH KEY doc_contabil = v_campo.

        IF wa_saida-doc_contabil IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD wa_saida-doc_contabil.
          SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'DOC_CONTABIL_E'.
        READ TABLE it_saida INTO wa_saida WITH KEY doc_contabil_e = v_campo.
        IF wa_saida-doc_contabil_e IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD wa_saida-doc_contabil_e.
          SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr_e.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'REVERSAO_DOC'. "130130 - CS2023000969 Gisele Follmann PSA
        READ TABLE it_saida INTO wa_saida WITH KEY reversao_doc = v_campo.
        IF wa_saida-reversao_doc IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD wa_saida-reversao_doc.
          SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr_e.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'DOC_LCTO'.
        READ TABLE it_saida INTO wa_saida WITH KEY doc_lcto = v_campo.
        IF wa_saida-doc_lcto IS NOT INITIAL.
          CLEAR vg_lote.
          "SET PARAMETER ID 'BLN' FIELD WA_SAIDA-DOC_LCTO.
          SET PARAMETER ID 'BLN' FIELD wa_saida-doc_lcto.
          SET PARAMETER ID 'LOT' FIELD  vg_lote.
          CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'DOC_LCTO_PREC'.
        READ TABLE it_saida INTO wa_saida WITH KEY doc_lcto_prec = v_campo.
        IF wa_saida-doc_lcto_prec IS NOT INITIAL.
          CLEAR vg_lote.
          SET PARAMETER ID 'BLN' FIELD wa_saida-doc_lcto_prec.
          SET PARAMETER ID 'LOT' FIELD vg_lote.
          CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'BELNR_PREC'.
        READ TABLE it_saida INTO wa_saida WITH KEY belnr_prec = v_campo.
        IF wa_saida-belnr_prec IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD wa_saida-belnr_prec.
          SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr_e.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.

    ENDCASE.

  ENDIF.


ENDFORM.                    "USER_COMMAND
"USER_COMMAND

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
FORM f_monta_top_of_page USING p_list_top_of_page TYPE
                               slis_t_listheader.

  DATA: t_header   TYPE slis_listheader,
        v_data(10) TYPE c.

  t_header-typ  = 'H'.
*  T_HEADER-INFO = text-t02 (T01).
  t_header-info = TEXT-t02 .
  APPEND t_header TO p_list_top_of_page.
  CLEAR t_header.
  WRITE sy-datum USING EDIT  MASK '__.__.____' TO v_data.
  CONCATENATE 'Data : '(023)  v_data INTO t_header-key SEPARATED BY
  space.
  t_header-typ  = 'S'.
  APPEND t_header TO p_list_top_of_page.

ENDFORM.                    " f_monta_top_of_page

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo  TYPE c
                               p_desc   TYPE c
                               p_tam    TYPE c
                               p_hot    TYPE c
                               p_zero   TYPE c
                               p_soma   TYPE c.

  DATA: wl_fcat TYPE ty_estrutura.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-do_sum    = p_soma.
  IF p_campo = 'ICON'.
    wl_fcat-icon      = 'X'.
  ENDIF.

  seq = seq + 1.
  wl_fcat-col_pos    = seq.

  APPEND wl_fcat TO it_fcat.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves .
  DATA:
    w_texto1(10),
    w_texto2(10),
    w_texto3(40),

    w_empresa_texto(40),
    w_exer_texto(40),
    w_per_texto(40),

    empresa             TYPE c LENGTH 99,
    lote                TYPE c LENGTH 99,
    cabec               TYPE c LENGTH 50.


  v_report = sy-repid.

*  W_TEXTO3 = 'Lotes Lançamentos Manuais'.
  w_texto3 = TEXT-t03.
  PERFORM f_construir_cabecalho USING 'H' w_texto3.
  SELECT SINGLE bukrs butxt
         FROM t001
         INTO wa_t001
  WHERE bukrs IN p_bukrs.
  IF p_bukrs IS NOT INITIAL.
*    W_EMPRESA_TEXTO = 'Empresa    :'.
    w_empresa_texto = TEXT-t01.
    CONCATENATE w_empresa_texto p_bukrs-low '-' wa_t001-butxt INTO empresa SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' empresa.
  ENDIF.

  IF p_lote IS NOT INITIAL.
*    W_EXER_TEXTO = 'Lote  :'.
    w_exer_texto = TEXT-t04.
    CONCATENATE w_exer_texto p_lote-low '-' p_lote-high  INTO lote SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' lote.
  ENDIF.


ENDFORM.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  f_construir_cabecalho
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TYP        text
*      -->TEXT       text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho    USING typ text.


  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

*&---------------------------------------------------------------------*
*&      Module  SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_lote INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_lote OCCURS 0,
          lote       TYPE zglt034-lote,
          descr_lote TYPE zglt034-descr_lote,
          bukrs      TYPE zglt034-bukrs,
        END OF tl_lote.


  SELECT lote descr_lote bukrs
    FROM zglt034
    INTO TABLE tl_lote
  WHERE status_lote NE 'A'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LOTE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT034-LOTE'
      value_org       = 'S'
    TABLES
      value_tab       = tl_lote
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF v_grava IS INITIAL.
    SET PF-STATUS '0100'.
  ELSE.
    SET PF-STATUS '0110'.
  ENDIF.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: w_answer(1),
        wa_zglt090  TYPE zglt090,
        it_zglt090  TYPE TABLE OF zglt090.

  CASE ok-code.
    WHEN 'SAIR' OR 'EXIT'.
      SET SCREEN 0.
    WHEN 'GRAVAR'.
      LOOP AT tg_estra INTO wg_estra.
        MOVE-CORRESPONDING wg_estra TO wa_zglt090.
        wa_zglt090-data_atual = sy-datum.
        wa_zglt090-hora_atual = sy-uzeit.
        wa_zglt090-usuario    = sy-uname.
        IF wg_estra-opcoes = icon_system_undo .
          w_answer = 'X'.
        ENDIF.
        APPEND wa_zglt090 TO it_zglt090.
      ENDLOOP.

      IF  w_answer = 'X'.
        CLEAR w_answer.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = TEXT-032
            text_button_1         = TEXT-033
            icon_button_1         = 'ICON_OKAY '
            text_button_2         = TEXT-034
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
            start_column          = 25
            start_row             = 6
          IMPORTING
            answer                = w_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF w_answer = '2'. "não
          EXIT.
        ENDIF.
      ENDIF.


      DELETE FROM zglt038 WHERE lote = wa_saida-lote.
      DELETE FROM zglt090 WHERE lote = wa_saida-lote.
      MODIFY zglt090 FROM TABLE it_zglt090.
      COMMIT WORK.
      MESSAGE s836(sd) WITH TEXT-035 wa_saida-lote TEXT-036.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: "EVENT TYPE CNTL_SIMPLE_EVENT,
    "EVENTS TYPE CNTL_SIMPLE_EVENTS,
    tl_filter   TYPE lvc_t_filt,
    wl_filter   TYPE lvc_s_filt,
    tl_function TYPE ui_functions,
    wl_function LIKE tl_function WITH HEADER LINE.

  wa_layout-zebra      = 'X'.
  wa_layout-no_toolbar = 'X'.
  wa_layout-no_rowmark = 'X'.
  wa_stable-row        = 'X'.
  wa_layout-grid_title = ' '.

  "GRID2
  IF obg_conteiner_estra IS INITIAL.
    CREATE OBJECT obg_conteiner_estra
      EXPORTING
        container_name = g_cc_estra.


    CREATE OBJECT grid2
      EXPORTING
        i_parent = obg_conteiner_estra.


    PERFORM montar_layout_estra.

    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
*    WA_LAYOUT-GRID_TITLE = 'Estratégia de Liberação'.
    wa_layout-grid_title = TEXT-t05.
    wa_layout-no_toolbar = c_x.
    PERFORM montar_layout_estra.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_estra[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_estra .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZGLT037'    'NIVEL'       'TG_ESTRA'    'NIVEL'              TEXT-p02               '05' ' ' ' ' ' ',
        1 'ZGLT037'    'VALOR_DE'    'TG_ESTRA'    'VALOR_DE'           TEXT-a01               '15' ' ' ' ' ' ',
        1 'ZGLT037'    'VALOR_ATE'   'TG_ESTRA'    'VALOR_ATE'          TEXT-a02               '15' ' ' ' ' ' ',
        1 'ZGLT037'    'APROVADOR'   'TG_ESTRA'    'APROVADOR'          TEXT-a03               '20' ' ' ' ' ' ',
        1 'ZGLT037'    'WAERS'       'TG_ESTRA'    'WAERS'              TEXT-a06               '20' ' ' ' ' ' ',
        1 ' '          'ESTADO'      'TG_ESTRA'    'ESTADO'             TEXT-a04               '10' ' ' ' ' ' ',
        1 ' '          'OPCOES'      'TG_ESTRA'    'OPCOES'             TEXT-a05               '12' ' ' ' ' ' '.



ENDFORM.                    " MONTAR_LAYOUT_ESTRA

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_COL_POS)        text
*      -->VALUE(P_REF_TABNAME)    text
*      -->VALUE(P_REF_FIELDNAME)  text
*      -->VALUE(P_TABNAME)        text
*      -->VALUE(P_FIELD)          text
*      -->VALUE(P_SCRTEXT_L)      text
*      -->VALUE(P_OUTPUTLEN)      text
*      -->VALUE(P_EDIT)           text
*      -->VALUE(P_SUM)            text
*      -->VALUE(P_EMPHASIZE)      text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            p_scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  IF p_field EQ 'OPCOES'. " OR P_FIELD EQ 'DOC_LCTO'.
    w_fieldcatalog-hotspot = c_x.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  ROW_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM row_selection .


  DATA: var_answer TYPE c.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD grid2->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

ENDFORM.

*USER STORY 163210 - MMSILVA - 15.01.2025 - Inicio
FORM f_alv_variant_f4 CHANGING pa_vari.

  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc = 0.
    p_layout = rs_variant-variant.
  ENDIF.

ENDFORM.
*USER STORY 163210 - MMSILVA - 15.01.2025 - Fim
