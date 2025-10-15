*&---------------------------------------------------------------------*
*& Report  ZSDR0009
*&
*&---------------------------------------------------------------------*
*&  carga das informações de exportação na tabela ITG_NF_EXPORT,
*&  esse programa deve ser executado antes da interface de RFC
*&---------------------------------------------------------------------*

REPORT  zsdr0099.

TYPE-POOLS: icon.

TABLES : zdde.

TYPES: BEGIN OF ty_/pwsati/zsati030.
         INCLUDE TYPE /pwsati/zsati030.
         TYPES: bacen      TYPE c LENGTH 120,
         pais_dest  TYPE n LENGTH 4,
         num_conhec TYPE c LENGTH 120,
         tp_conhect TYPE c LENGTH 120,
         data_emb   TYPE c LENGTH 255,
*       pais_dest_export type c length 4,
       END OF ty_/pwsati/zsati030.

TYPES: BEGIN OF ty_znom_conhec,
         id_conhec        TYPE znom_conhec-id_conhec,
         id_nomeacao_tran TYPE znom_conhec-id_nomeacao_tran,
         sg_pais_destino  TYPE znom_conhec-sg_pais_destino,
         nr_conhec        TYPE znom_conhec-nr_conhec,
         ds_tipo          TYPE znom_conhec-ds_tipo,
         dt_data          TYPE znom_conhec-dt_data,
         land1            TYPE zpais-land1,
       END OF ty_znom_conhec,

       BEGIN OF ty_vbrp ,
         vbeln TYPE j_1bnflin-refkey,
         vgbel TYPE vbrp-vgbel,
         netwr TYPE vbrp-netwr,
       END   OF ty_vbrp,

       BEGIN OF ty_zreg_exportacao,
         id_registro_expo TYPE zreg_exportacao-id_registro_expo,
         id_nomeacao_tran TYPE znom_transporte-id_nomeacao_tran,
         nr_valor_total   TYPE zreg_exportacao-nr_valor_total,
         dt_registro_expo TYPE zreg_exportacao-dt_registro_expo,
         id_pais_destino  TYPE zreg_exportacao-id_pais_destino,
       END   OF ty_zreg_exportacao.

DATA: t_j_1bnfdoc        TYPE TABLE OF j_1bnfdoc,
      t_j_1bnflin        TYPE TABLE OF j_1bnflin,
      t_zdoc_exp         TYPE TABLE OF zdoc_exp,
      t_vbrp             TYPE TABLE OF ty_vbrp,
      t_vbrp_aux         TYPE TABLE OF vbrp,
      t_zreg_exportacao  TYPE TABLE OF ty_zreg_exportacao,
      t_zreg_export_aux  TYPE TABLE OF zreg_exportacao,
      t_zdoc_rem_bl      TYPE TABLE OF zdoc_rem_bl,
      t_znom_transporte  TYPE TABLE OF znom_transporte,
      t_zdde_aplicacao   TYPE TABLE OF zdde_aplicacao,
      t_zdde             TYPE TABLE OF zdde,
      t_zsdt0170         TYPE TABLE OF zsdt0170,
      t_zsdt0170_ref     TYPE TABLE OF zsdt0170,
      t_zsdt0170_retific TYPE TABLE OF zsdt0170,
      t_zsdt0172         TYPE TABLE OF zsdt0172,
      t_zsdt0174         TYPE TABLE OF zsdt0174,
      t_znom_conhec      TYPE TABLE OF ty_znom_conhec,
      t_zpais            TYPE TABLE OF zpais,
      t_zdoc_memorando   TYPE TABLE OF zdoc_memorando,
      t_/pwsati/zsati030 TYPE TABLE OF ty_/pwsati/zsati030.

DATA: wa_j_1bnfdoc        TYPE j_1bnfdoc,
      wa_j_1bnflin        TYPE j_1bnflin,
      wa_zdoc_exp         TYPE zdoc_exp,
      wa_vbrp             TYPE ty_vbrp,
      wa_vbrp_aux         TYPE vbrp,
      wa_zreg_exportacao  TYPE ty_zreg_exportacao,
      wa_zreg_export_aux  TYPE zreg_exportacao,
      wa_zdoc_rem_bl      TYPE zdoc_rem_bl,
      wa_znom_transporte  TYPE znom_transporte,
      wa_zdde_aplicacao   TYPE zdde_aplicacao,
      wa_zdde             TYPE zdde,
      wa_zsdt0170         TYPE zsdt0170,
      wa_zsdt0172         TYPE zsdt0172,
      wa_zsdt0174         TYPE zsdt0174,
      wa_znom_conhec      TYPE ty_znom_conhec,
      wa_znom_conhec_aux  TYPE znom_conhec,
      wa_zpais            TYPE zpais,
      wa_zdoc_memorando   TYPE zdoc_memorando,
      wa_/pwsati/zsati030 TYPE ty_/pwsati/zsati030.

DATA: vg_txjcd           LIKE t001w-txjcd.

*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*
DATA:
  wa_cont   TYPE REF TO cl_gui_custom_container,
  wa_alv    TYPE REF TO cl_gui_alv_grid,
  wa_layout TYPE lvc_s_layo.

DATA: it_fcat            TYPE TABLE OF lvc_s_fcat,
      gs_alv_refres_cond TYPE lvc_s_stbl,
      formapgto          TYPE zib_contabil-zlsch,
      bcoempresa         TYPE zib_contabil-hbkid,
      dtvencto           TYPE zib_contabil-zfbdt,
      proc_v             TYPE c LENGTH 1,
      gs_variant_c       TYPE disvariant.


*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_dt_av FOR zdde-dt_averbacao.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  DELETE FROM /pwsati/zsati030.
  COMMIT WORK.

  PERFORM : zseleciona_dados,
            zprocessa_dados_dde,
            zprocessa_dados_due,

            zalv. " Form ALV.

  CALL SCREEN 0100.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  zseleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zseleciona_dados.
  DATA: tl_re TYPE RANGE OF znr_reg_ex WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_znom_conhec> TYPE ty_znom_conhec,
                 <fs_zdoc_exp>    TYPE zdoc_exp.

*---------------------------------------------------------*
* Busca Dados DU-e
*---------------------------------------------------------*
  SELECT *
    FROM zsdt0170 AS a INTO TABLE t_zsdt0170
   WHERE situacao_due  EQ '70' "Averbada
     AND dt_situacao   IN p_dt_av
     AND performance   EQ abap_false
     AND EXISTS ( SELECT *
                    FROM zdoc_exp AS b
                   WHERE b~id_due = a~id_due ).

  IF t_zsdt0170[] IS NOT INITIAL.

    "Paises DU-e
    SELECT *
      FROM zsdt0174 INTO TABLE t_zsdt0174
       FOR ALL ENTRIES IN t_zsdt0170
     WHERE id_due EQ t_zsdt0170-id_due.

    SORT t_zsdt0174 BY id_due destino_country.
    DELETE ADJACENT DUPLICATES FROM t_zsdt0174 COMPARING id_due.

    LOOP AT t_zsdt0170 INTO wa_zsdt0170 WHERE id_due_ref IS NOT INITIAL. "Caso tenha DU-e's com referencia

      SELECT *
        FROM zsdt0170 INTO TABLE t_zsdt0170_ref
         FOR ALL ENTRIES IN t_zsdt0170
       WHERE id_due = t_zsdt0170-id_due_ref.

      EXIT.
    ENDLOOP.

    LOOP AT t_zsdt0170_ref INTO DATA(_wl_0170_ref).
      READ TABLE t_zsdt0170 ASSIGNING FIELD-SYMBOL(<fs_zsdt0170>) WITH KEY id_due_ref = _wl_0170_ref-id_due.
      CHECK ( sy-subrc EQ 0 ).

      IF <fs_zsdt0170>-dt_registro_portal IS INITIAL.
        <fs_zsdt0170>-dt_registro_portal = _wl_0170_ref-dt_registro_portal.
      ENDIF.
    ENDLOOP.

  ENDIF.

*---------------------------------------------------------*
* Busca Dados DDE/RE
*---------------------------------------------------------*
  SELECT *
    INTO TABLE t_zdde
    FROM zdde
   WHERE dt_averbacao IN p_dt_av.

  IF t_zdde[] IS NOT INITIAL.
    SELECT *
      INTO TABLE t_zdde_aplicacao
      FROM zdde_aplicacao
       FOR ALL ENTRIES IN t_zdde
     WHERE id_dde EQ t_zdde-id_dde.

    IF t_zdde_aplicacao[] IS NOT INITIAL.
      SELECT *
        INTO TABLE t_zreg_export_aux
        FROM zreg_exportacao
         FOR ALL ENTRIES IN t_zdde_aplicacao
       WHERE id_registro_expo = t_zdde_aplicacao-id_registro_expo
         AND in_performance   = ''. "17.04.2017 CS2017000801
    ENDIF.
  ENDIF.

  LOOP AT t_zreg_export_aux INTO wa_zreg_export_aux.
    MOVE-CORRESPONDING wa_zreg_export_aux TO wa_zreg_exportacao.
    wa_zreg_exportacao-id_nomeacao_tran = wa_zreg_export_aux-id_nomeacao_tran.

    APPEND wa_zreg_exportacao TO t_zreg_exportacao.
  ENDLOOP.

  "Carregar Nomeação Transporte
  IF t_zreg_exportacao[] IS NOT INITIAL.
    SELECT *
      INTO TABLE t_znom_transporte
      FROM znom_transporte
       FOR ALL ENTRIES IN t_zreg_exportacao
     WHERE id_nomeacao_tran EQ t_zreg_exportacao-id_nomeacao_tran.
  ENDIF.

  IF t_zsdt0170[] IS NOT INITIAL.
    SELECT *
      APPENDING TABLE t_znom_transporte
      FROM znom_transporte
       FOR ALL ENTRIES IN t_zsdt0170
     WHERE id_nomeacao_tran EQ t_zsdt0170-id_nomeacao_tran.
  ENDIF.

  SORT t_znom_transporte BY id_nomeacao_tran.
  DELETE ADJACENT DUPLICATES FROM t_znom_transporte COMPARING id_nomeacao_tran.

  "Carrega Documentos Exportação
  IF t_zreg_exportacao[] IS NOT INITIAL.
    SELECT DISTINCT *
      INTO TABLE t_zdoc_exp
      FROM zdoc_exp AS a
       FOR ALL ENTRIES IN t_zreg_exportacao
     WHERE id_registro_expo = t_zreg_exportacao-id_registro_expo
       AND NOT EXISTS ( SELECT *
                          FROM zdoc_exp_recusa AS b
                         WHERE b~id_doc_exp = a~id_doc_exp ).
  ENDIF.

  IF t_zsdt0170[] IS NOT INITIAL.
    SELECT DISTINCT *
      FROM zdoc_exp AS a APPENDING TABLE t_zdoc_exp
       FOR ALL ENTRIES IN t_zsdt0170
     WHERE id_due = t_zsdt0170-id_due
       AND NOT EXISTS ( SELECT *
                          FROM zdoc_exp_recusa AS b
                         WHERE b~id_doc_exp = a~id_doc_exp ).
  ENDIF.

  IF t_zdoc_exp[] IS NOT INITIAL.
    FREE: tl_re.

    LOOP AT t_zdoc_exp ASSIGNING <fs_zdoc_exp>.

      CHECK <fs_zdoc_exp>-nr_registro_expo IS NOT INITIAL.

      CLEAR tl_re.
      tl_re-low    = <fs_zdoc_exp>-nr_registro_expo.
      REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN tl_re-low WITH '' IGNORING CASE.
      tl_re-sign   = 'I'.
      tl_re-option = 'EQ'.
      APPEND tl_re.

    ENDLOOP.

    UNASSIGN <fs_zdoc_exp>.

** Encontrar números de memorando
    IF tl_re[] IS NOT INITIAL.
      SELECT DISTINCT *
        INTO TABLE t_zdoc_memorando
        FROM zdoc_memorando
       WHERE nr_re IN tl_re.
    ENDIF.

    SELECT *
      INTO TABLE t_zdoc_rem_bl
      FROM zdoc_rem_bl
       FOR ALL ENTRIES IN t_zdoc_exp
     WHERE id_doc_exp EQ t_zdoc_exp-id_doc_exp.

    SELECT id_conhec id_nomeacao_tran sg_pais_destino nr_conhec ds_tipo dt_data
      INTO TABLE t_znom_conhec
      FROM znom_conhec
       FOR ALL ENTRIES IN t_zdoc_exp
     WHERE id_nomeacao_tran EQ t_zdoc_exp-id_nomeacao_tran.


    "Carregar Pais
    IF t_zreg_exportacao[] IS NOT INITIAL.

      SELECT *
        INTO TABLE t_zpais
        FROM zpais
         FOR ALL ENTRIES IN t_zreg_exportacao
       WHERE land1 EQ t_zreg_exportacao-id_pais_destino.

    ENDIF.

    IF t_zsdt0174[] IS NOT INITIAL.

      SELECT *
        APPENDING TABLE t_zpais
        FROM zpais
         FOR ALL ENTRIES IN t_zsdt0174
       WHERE land1 EQ t_zsdt0174-destino_country.

    ENDIF.

    SELECT *
      INTO TABLE t_vbrp_aux
      FROM vbrp
       FOR ALL ENTRIES IN t_zdoc_exp
     WHERE vgbel EQ t_zdoc_exp-vbeln AND DRAFT = SPACE .

    LOOP AT t_vbrp_aux INTO wa_vbrp_aux.
      MOVE-CORRESPONDING wa_vbrp_aux TO wa_vbrp.
      wa_vbrp-vbeln = wa_vbrp_aux-vbeln.

      APPEND wa_vbrp TO t_vbrp.

    ENDLOOP.

    IF sy-subrc IS INITIAL.
      SELECT *
        INTO TABLE t_j_1bnflin
        FROM j_1bnflin
         FOR ALL ENTRIES IN t_vbrp
       WHERE refkey = t_vbrp-vbeln.

      SELECT *
        INTO TABLE t_j_1bnfdoc
        FROM j_1bnfdoc
         FOR ALL ENTRIES IN t_j_1bnflin
       WHERE docnum = t_j_1bnflin-docnum.

    ENDIF.
  ENDIF.
ENDFORM.                    "zseleciona_dados

*&---------------------------------------------------------------------*
*&      Form  zprocessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zprocessa_dados_dde.
  DATA: vl_data_emb TYPE c LENGTH 10.

  SORT: t_zdde_aplicacao  BY id_dde,
        t_zreg_exportacao BY id_registro_expo,
        t_znom_transporte BY id_nomeacao_tran,
        t_zdoc_exp        BY id_registro_expo,
        t_zdoc_rem_bl     BY id_doc_exp,
        t_znom_conhec     BY id_nomeacao_tran ,
        t_vbrp            BY vgbel,
        t_j_1bnflin       BY refkey,
        t_j_1bnfdoc       BY docnum.

  LOOP AT t_zdde INTO wa_zdde.

  "PSA
    WRITE: wa_zdde-dt_dde       TO wa_/pwsati/zsati030-ixp_dt_declarac,
           wa_zdde-dt_averbacao TO wa_/pwsati/zsati030-ixp_dt_averb_dec,
           wa_zdde-nr_dde       TO wa_/pwsati/zsati030-ixp_num_dse.

    MOVE: wa_zdde-nr_valor     TO wa_/pwsati/zsati030-ixp_vlr_desp.

    LOOP AT t_zdde_aplicacao INTO wa_zdde_aplicacao WHERE id_dde = wa_zdde-id_dde .
      CLEAR : wa_zreg_exportacao,
              wa_znom_transporte,
              wa_zdoc_exp,
              wa_zdoc_rem_bl,
              wa_znom_conhec,
              wa_zpais,wa_vbrp,
              wa_j_1bnflin,
              wa_j_1bnfdoc.

      READ TABLE t_zreg_exportacao INTO wa_zreg_exportacao WITH KEY id_registro_expo = wa_zdde_aplicacao-id_registro_expo BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_zreg_exportacao-id_pais_destino = wa_zreg_exportacao-id_pais_destino.
      ENDIF.

      "PSA
      WRITE: wa_zreg_exportacao-dt_registro_expo TO wa_/pwsati/zsati030-ixp_dt_registro.


      LOOP AT t_zdoc_exp INTO wa_zdoc_exp WHERE id_registro_expo = wa_zreg_exportacao-id_registro_expo.
        wa_/pwsati/zsati030-ixp_num_dse    = wa_zdoc_exp-nr_registro_expo.

        REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_zdoc_exp-nr_registro_expo WITH '' IGNORING CASE.
        READ TABLE t_zdoc_memorando INTO wa_zdoc_memorando WITH KEY nr_re = wa_zdoc_exp-nr_registro_expo.
        IF sy-subrc IS INITIAL.
          wa_/pwsati/zsati030-ixp_num_memo = wa_zdoc_memorando-numero_memo.
        ENDIF.

        READ TABLE t_vbrp INTO wa_vbrp WITH KEY vgbel = wa_zdoc_exp-vbeln BINARY SEARCH.
        MOVE : wa_vbrp-netwr     TO wa_/pwsati/zsati030-ixp_vlr_re.

        READ TABLE t_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_vbrp-vbeln BINARY SEARCH.


        READ TABLE t_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.
        wa_/pwsati/zsati030-ixp_docnum = |{ wa_j_1bnfdoc-docnum ALPHA = IN }|.

        CLEAR: wa_/pwsati/zsati030-pais_dest,
               wa_/pwsati/zsati030-num_conhec,
               wa_/pwsati/zsati030-tp_conhect,
               wa_/pwsati/zsati030-data_emb.

        LOOP AT t_znom_conhec INTO wa_znom_conhec WHERE id_nomeacao_tran EQ wa_zdoc_exp-id_nomeacao_tran.
          CLEAR wa_zpais.

          READ TABLE t_zpais INTO wa_zpais WITH KEY land1 = wa_zreg_exportacao-id_pais_destino.
          IF sy-subrc IS INITIAL.
            wa_/pwsati/zsati030-pais_dest     =  wa_zpais-bacen(4).
            wa_/pwsati/zsati030-ixp_pais_dest =  wa_/pwsati/zsati030-pais_dest.
          ENDIF.

          IF wa_/pwsati/zsati030-num_conhec IS NOT INITIAL.
            CONCATENATE wa_/pwsati/zsati030-num_conhec wa_znom_conhec-nr_conhec INTO wa_/pwsati/zsati030-num_conhec SEPARATED BY ','.
          ELSE.
            wa_/pwsati/zsati030-num_conhec = wa_znom_conhec-nr_conhec.
          ENDIF.

          REPLACE ALL OCCURRENCES OF '/' IN wa_znom_conhec-ds_tipo WITH ''.
          CONDENSE wa_znom_conhec-ds_tipo NO-GAPS.
          IF sy-subrc IS NOT INITIAL.
            wa_/pwsati/zsati030-tp_conhect = wa_znom_conhec-ds_tipo.
          ENDIF.

          CONCATENATE wa_znom_conhec-dt_data+6(2) wa_znom_conhec-dt_data+4(2) wa_znom_conhec-dt_data(4) INTO vl_data_emb SEPARATED BY '.'.
          IF wa_/pwsati/zsati030-data_emb IS NOT INITIAL.
            CONCATENATE wa_/pwsati/zsati030-data_emb vl_data_emb INTO wa_/pwsati/zsati030-data_emb   SEPARATED BY ','.
          ELSE.
            wa_/pwsati/zsati030-data_emb = vl_data_emb.
          ENDIF.
        ENDLOOP.
        """""""""
        READ TABLE t_znom_conhec INTO wa_znom_conhec WITH KEY id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.
        READ TABLE t_zreg_exportacao INTO wa_zreg_exportacao WITH KEY id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.
        IF sy-subrc IS INITIAL.
          READ TABLE t_zpais INTO wa_zpais WITH KEY land1 = wa_zreg_exportacao-id_pais_destino.
          IF sy-subrc IS INITIAL.
            wa_/pwsati/zsati030-pais_dest = wa_zpais-bacen(4).
            wa_/pwsati/zsati030-ixp_pais_dest = wa_/pwsati/zsati030-pais_dest.
          ENDIF.

          REPLACE ALL OCCURRENCES OF '/' IN wa_znom_conhec-ds_tipo WITH ''.

*          22	Mate s Receipt
*          01	AWB
*          02	MAWB
*          03	HAWB
*          04	COMAT
*          06	R. EXPRESSAS
*          07	ETIQ. REXPRESSAS
*          08	HR. EXPRESSAS
*          09	AV7
*          10	BL
*          11	MBL
*          12	HBL
*          13	CRT
*          14	DSIC
*          16	COMAT BL
*          17	RWB
*          18	HRWB
*          19	TIF/DTA
*          20	CP2
*          91	NÂO IATA
*          92	MNAO IATA
*          93	HNAO IATA
*          99	OUTROS

          CASE wa_znom_conhec-ds_tipo.
            WHEN 'BL'.
              wa_/pwsati/zsati030-ixp_conhec_tp = 'BL'.
            WHEN 'CTR'.
              wa_/pwsati/zsati030-ixp_conhec_tp = 'CRT'.
          ENDCASE.

          wa_/pwsati/zsati030-ixp_num_conh_emb   = wa_znom_conhec-nr_conhec.
        ENDIF.

        wa_/pwsati/zsati030-ixp_cod_moeda = 'USD'.


        MODIFY /pwsati/zsati030 FROM wa_/pwsati/zsati030.

        APPEND wa_/pwsati/zsati030 TO t_/pwsati/zsati030.

      ENDLOOP.
    ENDLOOP.

  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    "zprocessa


*&---------------------------------------------------------------------*
*&      Form  ZALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zalv.
  PERFORM alv_preenche_cat USING:


      'IXP_DOCNUM'               'IXP_DOCNUM'                  '15'       ' '  ' '     ' ' ,
      'IXP_COD_MOEDA'            'IXP_COD_MOEDA'               '15'       ' '  'X'     ' ' ,
      'IXP_NUM_RE'               'IXP_NUM_RE'                  '15'       ' '  'X'     ' ' ,
      'IXP_VLR_RE'               'IXP_VLR_RE'                  '15'       ' '  'X'     ' ' ,
      'IXP_VLR_DESP'             'IXP_VLR_DESP'                '15'       ' '  'X'     ' ' ,
      'IXP_NUM_DESP'             'IXP_NUM_DESP'                '15'       ' '  'X'     ' ' ,
      'IXP_DT_EMB'               'IXP_DT_EMB'                  '15'       ' '  ''      ' ' ,
      'IXP_PAIS_DEST'            'IXP_PAIS_DEST'               '15'       ' '  'X'     ' ' ,
      'IXP_NUM_DSE'              'IXP_NUM_DSE'                 '15'       ' '  'X'     ' ' ,
      'IXP_DT_DOCTO'             'IXP_DT_DOCTO'                '15'       ' '  'X'     ' ' ,
      'IXP_NUM_COURR'            'IXP_NUM_COURR'               '15'       ' '  'X'     ' ' ,
      'IXP_CONHEC_TP'            'IXP_CONHEC_TP'               '15'       ' '  'X'     ' ' ,
      'IXP_SISCOMEX_TP'          'IXP_SISCOMEX_TP'             '15'       ' '  'X'     ' ' ,
      'IXP_DT_DECLARAC'          'IXP_DT_DECLARAC'             '15'       ' '  'X'     ' ' ,
      'IXP_DT_REGISTRO'          'IXP_DT_REGISTRO'             '15'       ' '  'X'     ' ' ,
      'IXP_DT_AVERB_DEC'         'IXP_DT_AVERB_DEC'            '15'       ' '  'X'     ' ' ,
      'IXP_DT_EMIS_MEMO'         'IXP_DT_EMIS_MEMO'            '15'       ' '  'X'     ' ' ,
      'IXP_NUM_CONH_EMB'         'IXP_NUM_CONH_EMB'            '15'       ' '  'X'     ' ' ,
      'IXP_NUM_MEMO'             'IXP_NUM_MEMO'                '15'       ' '  'X'     ' ' ,
      'IXP_NUM_DUE'              'IXP_NUM_DUE'                 '15'       ' '  'X'     ' ' .

*
*      'IXP_DOC_NUM'        'IXP_DOC_NUM'                 '15'       ' '  ' '     ' ' ,
*      'IXP_NUM_RE'         'IXP_NUM_RE'                  '15'       ' '  'X'     ' ' ,
*      'IXP_NUM_DESP'       'IXP_NUM_DESP'                '15'       ' '  'X'     ' ' ,
*      'IXP_NUM_MEMO'       'IXP_NUM_MEMO'                '15'       ' '  'X'     ' ' ,
*      'IXP_COD_MOEDA'      'IXP_COD_MOEDA'               '15'       ' '  'X'     ' ' ,
*      'IXP_VLR_DESP'       'IXP_VLR_DESP'                '15'       ' '  'X'     ' ' ,
*      'PAIS_DEST'          'IXP_PAIS_DEST'               '15'       ' '  ''      ' ' ,
*      'IXP_VLR_RE'         'IXP_VLR_RE'                  '15'       ' '  'X'     ' ' ,
*      'IXP_DT_DECLARAC'    'IXP_DT_DECLARAC'             '15'       ' '  'X'     ' ' ,
*      'IXP_DT_REGISTRO'    'IXP_DT_REGISTRO'             '15'       ' '  'X'     ' ' ,
*      'IXP_NUM_CONH_EMB'   'IXP_NUM_CONH_EMB'            '15'       ' '  'X'     ' ' ,
*      'IXP_CONHEC_TP'      'IXP_CONHEC_TP'               '15'       ' '  'X'     ' ' ,
*      'IXP_DT_AVERB_DEC'   'IXP_DT_AVERB_DEC'            '15'       ' '  'X'     ' ' .

ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c
                               p_mask  TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'T_/PWSATI/ZSATI030'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-edit_mask = p_mask.
  wl_fcat-outputlen = p_tam.

  "wl_fcat-convexit  = p_mask.
  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT


*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_status OUTPUT.
  DATA: wa_fcode TYPE sy-ucomm,
        it_fcode LIKE TABLE OF wa_fcode.

  SET PF-STATUS 'FF0100' EXCLUDING it_fcode.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: wa_event       TYPE REF TO  lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
*    PERFORM Z_HANDLE_HOTSPOT USING    E_ROW_ID
*                                      E_COLUMN_ID
*                                      ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos

    "PERFORM Z_HANDLE_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.



ENDFORM.                    " Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_exibe_alv OUTPUT.

  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.

  wa_layout-sel_mode = 'A'.

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant_c
      i_save                        = 'A'
    CHANGING
      it_outtab                     = t_/pwsati/zsati030
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT

FORM zprocessa_dados_due.
  DATA: vl_data_emb TYPE c LENGTH 10.

  SORT: t_znom_transporte BY id_nomeacao_tran,
        t_zdoc_exp        BY id_registro_expo,
        t_zdoc_rem_bl     BY id_doc_exp,
        t_znom_conhec     BY id_nomeacao_tran ,
        t_vbrp            BY vgbel,
        t_j_1bnflin       BY refkey,
        t_j_1bnfdoc       BY docnum,
        t_zsdt0174        BY id_due.

  LOOP AT t_zsdt0170 INTO wa_zsdt0170.

    CLEAR: wa_/pwsati/zsati030,
           wa_zsdt0174.

           "PSA a mesma data do portal é a dt_registro_portal do dt_leitura_opus
           if wa_zsdt0170-dt_registro_portal is INITIAL.
           clear: wa_zsdt0170-dt_registro_portal.
           wa_zsdt0170-dt_registro_portal = wa_zsdt0170-dt_leitura_opus.
           ENDIF.


    WRITE: wa_zsdt0170-dt_registro_portal  TO wa_/pwsati/zsati030-ixp_dt_declarac,
           wa_zsdt0170-dt_situacao         TO wa_/pwsati/zsati030-ixp_dt_averb_dec.

    wa_/pwsati/zsati030-ixp_num_desp = 0.


    READ TABLE t_zsdt0174 INTO wa_zsdt0174 WITH KEY id_due = wa_zsdt0170-id_due BINARY SEARCH.

           "PSA a mesma data do portal é a dt_registro_portal do dt_leitura_opus
           if wa_zsdt0170-dt_registro_portal is INITIAL.
           clear: wa_zsdt0170-dt_registro_portal.
           wa_zsdt0170-dt_registro_portal = wa_zsdt0170-dt_leitura_opus.
           ENDIF.

    WRITE: wa_zsdt0170-dt_registro_portal TO wa_/pwsati/zsati030-ixp_dt_registro.

    LOOP AT t_zdoc_exp INTO wa_zdoc_exp WHERE id_due = wa_zsdt0170-id_due.

      CLEAR: wa_vbrp, wa_j_1bnflin, wa_j_1bnfdoc.

      wa_/pwsati/zsati030-ixp_num_re    = wa_zdoc_exp-numero_due.

      REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN wa_zdoc_exp-numero_due WITH '' IGNORING CASE.

      READ TABLE t_vbrp INTO wa_vbrp WITH KEY vgbel = wa_zdoc_exp-vbeln BINARY SEARCH.
      MOVE : wa_vbrp-netwr     TO wa_/pwsati/zsati030-ixp_vlr_re.

      READ TABLE t_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_vbrp-vbeln BINARY SEARCH.

      READ TABLE t_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.

      wa_/pwsati/zsati030-ixp_docnum = |{ wa_j_1bnfdoc-docnum ALPHA = IN }|.

      CLEAR: wa_/pwsati/zsati030-pais_dest,
             wa_/pwsati/zsati030-num_conhec,
             wa_/pwsati/zsati030-tp_conhect,
             wa_/pwsati/zsati030-data_emb.

      LOOP AT t_znom_conhec INTO wa_znom_conhec WHERE id_nomeacao_tran EQ wa_zdoc_exp-id_nomeacao_tran.
        CLEAR wa_zpais.

        READ TABLE t_zpais INTO wa_zpais WITH KEY land1 = wa_zsdt0174-destino_country.
        IF sy-subrc IS INITIAL.
          wa_/pwsati/zsati030-pais_dest    = wa_zpais-bacen(4).
          wa_/pwsati/zsati030-ixp_pais_dest = wa_/pwsati/zsati030-pais_dest.
        ENDIF.

        IF wa_/pwsati/zsati030-num_conhec IS NOT INITIAL.
          CONCATENATE wa_/pwsati/zsati030-num_conhec wa_znom_conhec-nr_conhec INTO wa_/pwsati/zsati030-num_conhec SEPARATED BY ','.
        ELSE.
          wa_/pwsati/zsati030-num_conhec = wa_znom_conhec-nr_conhec.
        ENDIF.

        REPLACE ALL OCCURRENCES OF '/' IN wa_znom_conhec-ds_tipo WITH ''.
        CONDENSE wa_znom_conhec-ds_tipo NO-GAPS.
        IF sy-subrc IS NOT INITIAL.
          wa_/pwsati/zsati030-tp_conhect = wa_znom_conhec-ds_tipo.
        ENDIF.

        CONCATENATE wa_znom_conhec-dt_data+6(2) wa_znom_conhec-dt_data+4(2) wa_znom_conhec-dt_data(4) INTO vl_data_emb SEPARATED BY '.'.
        IF wa_/pwsati/zsati030-data_emb IS NOT INITIAL.
          CONCATENATE wa_/pwsati/zsati030-data_emb vl_data_emb INTO wa_/pwsati/zsati030-data_emb   SEPARATED BY ','.
        ELSE.
          wa_/pwsati/zsati030-data_emb = vl_data_emb.
        ENDIF.
      ENDLOOP.

      READ TABLE t_zpais INTO wa_zpais WITH KEY land1 = wa_zsdt0174-destino_country.
      IF sy-subrc IS INITIAL.
        wa_/pwsati/zsati030-pais_dest = wa_zpais-bacen(4).
        wa_/pwsati/zsati030-ixp_pais_dest = wa_/pwsati/zsati030-pais_dest.
      ENDIF.

      READ TABLE t_znom_conhec INTO wa_znom_conhec WITH KEY id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.
      IF sy-subrc IS INITIAL.
        REPLACE ALL OCCURRENCES OF '/' IN wa_znom_conhec-ds_tipo WITH ''.

*       22  Mate s Receipt
*       01  AWB
*       02  MAWB
*       03  HAWB
*       04  COMAT
*       06  R. EXPRESSAS
*       07  ETIQ. REXPRESSAS
*       08  HR. EXPRESSAS
*       09  AV7
*       10  BL
*       11  MBL
*       12  HBL
*       13  CRT
*       14  DSIC
*       16  COMAT BL
*       17  RWB
*       18  HRWB
*       19  TIF/DTA
*       20  CP2
*       91  NÂO IATA
*       92  MNAO IATA
*       93  HNAO IATA
*       99  OUTROS

        CASE wa_znom_conhec-ds_tipo.
          WHEN 'BL'.
            wa_/pwsati/zsati030-ixp_conhec_tp = 'BL'.
          WHEN 'CTR'.
            wa_/pwsati/zsati030-ixp_conhec_tp = 'CRT'.
        ENDCASE.

        wa_/pwsati/zsati030-ixp_num_conh_emb   = wa_znom_conhec-nr_conhec.
      ENDIF.

      MODIFY /pwsati/zsati030 FROM wa_/pwsati/zsati030.

      APPEND wa_/pwsati/zsati030 TO t_/pwsati/zsati030.

    ENDLOOP.

  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    "zprocessa_dados_due.
