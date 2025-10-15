
TABLES: t001, skb1, bsis, sscrfields, zfit0198, j_1bnfdoc, mara, zib_contabil, tzb27.

DATA: it_screen_status TYPE TABLE OF sy-ucomm.
DATA: functxt TYPE smp_dyntxt.

TYPES:
  BEGIN OF ty_saida1,
    bukrs     TYPE    j_1bnfdoc-bukrs,
    gjahr     TYPE    j_1bnfdoc-gjahr,
    poper     TYPE    acdoca-poper,
    itmnum    TYPE    j_1bnflin-itmnum,
    branch    TYPE    j_1bnfdoc-branch,
    cfop      TYPE    j_1bnflin-cfop,
    tp_m      TYPE    zegl_tpm,
    nfenum    TYPE    j_1bnfdoc-nfenum,
    docnum    TYPE    j_1bnfdoc-docnum,
    parid     TYPE    j_1bnfdoc-parid,
    name1     TYPE    kna1-name1,
    matkl     TYPE    j_1bnflin-matkl,
    rassc     TYPE    acdoca-rassc,
    shpunt    TYPE    j_1bnfdoc-shpunt,
    matnr     TYPE    j_1bnflin-matnr,
    maktx     TYPE    makt-maktx,
    pstdat    TYPE    j_1bnfdoc-pstdat,
    docdat    TYPE    j_1bnfdoc-docdat,
*    anzpk     TYPE    j_1bnfdoc-anzpk,
    anzpk     TYPE    decfloat34,i,"j_1bnflin-menge,
*    hsl       TYPE    acdoca-hsl,
    tsl       TYPE    acdoca-tsl,
    hsl       TYPE    acdoca-hsl,
    kursf     TYPE    bkpf-kursf,
    aubel     TYPE    vbrp-aubel,
    refkey    TYPE    j_1bnflin-refkey,
    belnr     TYPE    bkpf-belnr,
    ort01_p   TYPE    j_1bnfnad-ort01,
    regio     TYPE    j_1bnfnad-regio,
    ort01     TYPE    j_1bnfnad-ort01,
    regio_l   TYPE    j_1bnfnad-regio,
    inco1     TYPE    j_1bnfdoc-inco1,
    racct     TYPE    acdoca-racct,
    saknr_cc  TYPE    tzb27-saknr,
    saknr_r   TYPE    tzb27-saknr,
    saknr_rc  TYPE    tzb27-saknr,
    saknr_c   TYPE    tzb27-saknr,
    saknr_p   TYPE    tzb27-saknr,
    saknr_e   TYPE    tzb27-saknr,
    saknr_ec  TYPE    tzb27-saknr,
    waers     TYPE    bkpf-waers,
    uf        TYPE    char10,
    route     TYPE    vbap-route,
    traztd    TYPE    char10,
    docdat_e  TYPE    j_1bnfdoc-docdat,
    gera      TYPE    char3,
    nr_conhec TYPE   char20,
  END OF ty_saida1,

  BEGIN OF ty_saida2,
    rbukrs TYPE    acdoca-rbukrs,
    gjahr  TYPE    acdoca-gjahr,
    belnr  TYPE    acdoca-belnr,
    rassc  TYPE    acdoca-rassc,
    ksl    TYPE    acdoca-ksl,
    tsl    TYPE    acdoca-tsl,
    msl    TYPE    acdoca-msl,
    matnr  TYPE    acdoca-matnr,
    racct  TYPE    acdoca-racct,
    precob TYPE    acdoca-hsl,
    preco  TYPE    acdoca-hsl,
    poper  TYPE acdoca-poper,
  END OF ty_saida2,

  BEGIN OF ty_saida3,
    status_1  TYPE char4,
    status_2  TYPE char4,
    doc_rec   TYPE vbeln_va,
    doc_rev_r TYPE vbeln_va,
    doc_cust  TYPE vbeln_va,
    doc_rev_c TYPE vbeln_va.
    INCLUDE TYPE zglt0004.
TYPES: END OF ty_saida3.

DATA:
  lv_dia     TYPE sy-datum,
  lv_ult_dia TYPE sy-datum,
  p_poper    TYPE poper.

DATA:
  it_saida1   TYPE STANDARD TABLE OF ty_saida1 INITIAL SIZE 0,
  "wa_saida1 TYPE ty_saida1,
  it_saida2   TYPE STANDARD TABLE OF ty_saida2 INITIAL SIZE 0,
  "wa_saida2 TYPE ty_saida2,
  it_saida3   TYPE STANDARD TABLE OF ty_saida3 INITIAL SIZE 0,
  "wa_saida3 TYPE ty_saida3.
  it_zglt0003 TYPE STANDARD TABLE OF zglt0003 INITIAL SIZE 0.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS p_bukrs FOR t001-bukrs NO-EXTENSION NO INTERVALS.
  PARAMETERS p_mes TYPE month VALUE CHECK.
  PARAMETERS p_ano TYPE gjahr.
  SELECT-OPTIONS p_budat FOR bsis-budat NO-DISPLAY.
  SELECT-OPTIONS p_docnum FOR j_1bnfdoc-docnum. "NO-EXTENSION NO INTERVALS.
  PARAMETERS:p_gera(5) AS LISTBOX VISIBLE LENGTH 5 DEFAULT ''.
  SELECT-OPTIONS p_matnr FOR mara-matnr. "NO-EXTENSION NO INTERVALS.
  SELECT-OPTIONS p_waers FOR zib_contabil-waers NO-EXTENSION NO INTERVALS.
  SELECT-OPTIONS p_saknr FOR tzb27-saknr. "NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK part1.

PARAMETERS:
  p_cont RADIOBUTTON GROUP grp1 DEFAULT 'X', " Default selected
  p_rev  RADIOBUTTON GROUP grp1.

INITIALIZATION.


  TYPES: BEGIN OF ty_simnao,
           name TYPE char3,
           id   TYPE char1,
         END OF ty_simnao.

  DATA: list          TYPE vrm_values,
        value         LIKE LINE OF list,
        it_f1_sim_nao TYPE STANDARD TABLE OF ty_simnao INITIAL SIZE 0,
        p_pedagio     TYPE char3.

  SELECT name,id FROM zi_f1_sim_nao INTO TABLE @it_f1_sim_nao.

  LOOP AT it_f1_sim_nao INTO DATA(wa_f1_sim_nao).
    value-key = wa_f1_sim_nao-name.
    APPEND value TO list.
    CLEAR: wa_f1_sim_nao.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_GERA'
      values          = list
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.

  SELECTION-SCREEN : FUNCTION KEY 1.
*  sscrfields-functxt_01 = 'Cadastro de CFOP'.
  functxt-icon_id = icon_submit.
  functxt-quickinfo = 'Cadastro de CFOP'.
  functxt-icon_text = 'Cadastro de CFOP'.
  sscrfields-functxt_01 = functxt.

  SELECTION-SCREEN : FUNCTION KEY 2.
*  sscrfields-functxt_02 = 'Cadastro de Sociedade Parceira'.
  FREE functxt.
  functxt-icon_id = icon_submit.
  functxt-quickinfo = 'Cadastro de Sociedade Parceira'.
  functxt-icon_text = 'Cadastro de Sociedade Parceira'.
  sscrfields-functxt_02 = functxt.

  SELECTION-SCREEN : FUNCTION KEY 3.
*  sscrfields-functxt_03 = 'De/Para Contas CutOff'.
  FREE functxt.
  functxt-icon_id = icon_submit.
  functxt-quickinfo = 'De/Para Contas CutOff'.
  functxt-icon_text = 'De/Para Contas CutOff'.
  sscrfields-functxt_03 = functxt.



*AT SELECTION-SCREEN OUTPUT. Caso queira amarrar a um status os icones!
*
*  it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).
*
*  IF sy-dynnr = 1000.
*
*    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
*      EXPORTING
*        p_status  = 'STATUS_1000'
*        p_program = sy-repid
*      TABLES
*        p_exclude = it_screen_status.
*
*  ENDIF.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ONLI'.
*      LOOP AT SCREEN.
*        IF p_bukrs IS INITIAL.
**          IF screen-name(7) = 'P_BUKRS'.
**            screen-required = '2'.
**            MODIFY SCREEN.
**          ENDIF.
*        ENDIF.
*      ENDLOOP.

      IF p_ano IS INITIAL.
        MESSAGE 'Ano é obrigatórios!' TYPE 'E'.
        STOP.
      ELSE.
        IF p_ano GT sy-datum(4) OR p_ano LT 1500.
          MESSAGE 'Ano fora de validade!' TYPE 'E'.
          STOP.
        ENDIF.
      ENDIF.

      IF p_mes IS INITIAL.
        MESSAGE 'Mês é obrigatórios!' TYPE 'E'.
        STOP.
      ENDIF.

      IF p_bukrs IS INITIAL. " OR s_budat[] IS INITIAL.
        MESSAGE 'Empresa campo obrigatório!' TYPE 'E'.
        STOP.
      ENDIF.

      FREE:p_budat.
      CLEAR:lv_dia,lv_ult_dia,p_poper.

      lv_dia = p_ano && p_mes && '01'.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = lv_dia
        IMPORTING
          last_day_of_month = lv_ult_dia.

      p_budat-sign = 'I'.
      p_budat-option = 'BT'.
      p_budat-low = lv_dia.
      p_budat-high = lv_ult_dia.
      APPEND p_budat TO p_budat[].

      "p_budat[] = VALUE #( ( option = 'BT' sign = 'I' low = lv_dia high = lv_ult_dia ) ).

      p_poper = p_mes.

      IF p_matnr-low IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = p_matnr-low "campo de 400char
          IMPORTING
            output = p_matnr-low.

      ENDIF.

      IF p_saknr.
        CONDENSE p_saknr-low NO-GAPS.
      ENDIF.

      PERFORM get_data_start.

*      IF it_saida1 IS INITIAL.
*        MESSAGE 'Não foram encontrados registros com os parâmetros informado!' TYPE 'I' DISPLAY LIKE 'i'.
*        EXIT.
*      ELSE.
      CALL SCREEN 100.
*      ENDIF.

    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'FC01'.
      CALL TRANSACTION 'ZGL089'.

    WHEN 'FC02'.
      CALL TRANSACTION 'ZGL090'.

    WHEN 'FC03'.
      CALL TRANSACTION 'ZGL091'.

  ENDCASE.


START-OF-SELECTION.

  "Não usar porque começa no AT SELECTION-SCREEN. when 'ONLI' (Executa)
