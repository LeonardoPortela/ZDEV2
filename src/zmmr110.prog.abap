*&---------------------------------------------------------------------*
*& Report  ZMMR110
*&
*&---------------------------------------------------------------------*
*&
*&ANTONIO LUIZ RODRIGUES DA SILVA
*&06/01/2017
*&---------------------------------------------------------------------*
REPORT zmmr110.

TABLES: ckmlhd, ckmlpp, mlkey.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

TYPES:
  BEGIN OF ty_saida,
    niv00(1),
    nivel(10),
    nitxt(50),
    saknr     TYPE ska1-saknr,
    matnr     TYPE mara-matnr,
    maktx     TYPE makt-maktx,
    vlr_01    TYPE zwrbtr_dre,
    vlr_02    TYPE zwrbtr_dre,
    vlr_03    TYPE zwrbtr_dre,
    vlr_04    TYPE zwrbtr_dre,
    vlr_05    TYPE zwrbtr_dre,
    vlr_06    TYPE zwrbtr_dre,
    vlr_07    TYPE zwrbtr_dre,
    vlr_08    TYPE zwrbtr_dre,
    vlr_09    TYPE zwrbtr_dre,
    vlr_10    TYPE zwrbtr_dre,
    vlr_11    TYPE zwrbtr_dre,
    vlr_12    TYPE zwrbtr_dre,
  END OF ty_saida,

  BEGIN OF ty_bsis,
    belnr TYPE bsis-belnr,
    bukrs TYPE bsis-bukrs,
    hkont TYPE bsis-hkont,
    gjahr TYPE bsis-gjahr,
    budat TYPE bsis-budat,
    gsber TYPE bsis-gsber,
    dmbtr TYPE bsis-dmbtr,
    dmbe2 TYPE bsis-dmbe2,
    kostl TYPE bsis-kostl,
    shkzg TYPE bsis-shkzg,
  END OF ty_bsis,

  BEGIN OF ty_hkont,
    hkont TYPE bsis-hkont,
  END OF ty_hkont.



*----------------------------------------------------------------------*
* Objetos TREE
*----------------------------------------------------------------------*
DATA: tree1              TYPE REF TO cl_gui_alv_tree,
      lt_list_commentary TYPE slis_t_listheader,
      l_hierarchy_header TYPE treev_hhdr,
      gt_fieldcatalog    TYPE lvc_t_fcat,
      w_fieldcatalog     TYPE lvc_s_fcat,
      vg_rel(1).

DATA:
*---> 28/06/2023 - Migração S4 - JS
* VG_NIVEL_EST TYPE CHAR02,
  vg_nivel_est TYPE zchar02,
*<--- 28/06/2023 - Migração S4 - JS
  vg_bdatj     TYPE ckmlpp-bdatj,
  vg_poper     TYPE ckmlpp-poper.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: gt_saida     TYPE TABLE OF ty_saida,
      gt_saida_abe TYPE TABLE OF ty_saida,
      gt_saida_tot TYPE TABLE OF ty_saida,
      wa_saida_tot TYPE ty_saida,
      it_saida     TYPE TABLE OF ty_saida  WITH HEADER LINE,
      it_saida_aux TYPE TABLE OF ty_saida  WITH HEADER LINE,
      it_ckmlhd    TYPE TABLE OF ckmlhd,
      it_ckmlhd_um TYPE TABLE OF ckmlhd,
      it_ckmlpp    TYPE TABLE OF ckmlpp,
      it_ckmlpp_um TYPE TABLE OF ckmlpp,
      it_ckmlcr    TYPE TABLE OF ckmlcr,
      it_mlcd      TYPE TABLE OF mlcd,
      it_mlcd_tod  TYPE TABLE OF mlcd,
      it_makt      TYPE TABLE OF makt,
      it_skat      TYPE TABLE OF skat,
      it_bsis      TYPE TABLE OF bsis,
      it_zcot0010  TYPE TABLE OF zcot0010,
      t_hkont      TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      it_hkont     TYPE TABLE OF ty_hkont WITH HEADER LINE.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: ok-code       TYPE sy-ucomm,
      wa_saida      TYPE ty_saida,
      wa_saida_prd  TYPE ty_saida,
      wa_saida_acu  TYPE ty_saida,
      wa_saida_con  TYPE ty_saida,
      wa_saida_tra  TYPE ty_saida,
      wa_saida_fin  TYPE ty_saida,
      wa_saida_abe  TYPE ty_saida,

      wa_saida_a    TYPE ty_saida,
      wa_saida_b    TYPE ty_saida,
      wa_saida_c    TYPE ty_saida,
      wa_saida_d    TYPE ty_saida,
      wa_saida_e    TYPE ty_saida,
      wa_saida_f    TYPE ty_saida,
      wa_saida_g    TYPE ty_saida,
      wa_saida_h    TYPE ty_saida,
      wa_saida_i    TYPE ty_saida,
      wa_saida_j    TYPE ty_saida,
      wa_saida_k    TYPE ty_saida,
      wa_saida_l    TYPE ty_saida,
      wa_saida_m    TYPE ty_saida,

      wa_saida_a1   TYPE ty_saida,
      wa_saida_b1   TYPE ty_saida,
      wa_saida_c1   TYPE ty_saida,
      wa_saida_d1   TYPE ty_saida,
      wa_saida_e1   TYPE ty_saida,
      wa_saida_f1   TYPE ty_saida,
      wa_saida_g1   TYPE ty_saida,
      wa_saida_h1   TYPE ty_saida,
      wa_saida_i1   TYPE ty_saida,
      wa_saida_j1   TYPE ty_saida,
      wa_saida_k1   TYPE ty_saida,
      wa_saida_l1   TYPE ty_saida,
      wa_saida_m1   TYPE ty_saida,

      wa_ckmlhd     TYPE ckmlhd,
      wa_ckmlpp     TYPE ckmlpp,
      wa_ckmlcr     TYPE ckmlcr,
      wa_mlcd       TYPE mlcd,
      wa_makt       TYPE makt,
      wa_skat       TYPE skat,
      wa_bsis       TYPE bsis,
      wa_zcot0010   TYPE zcot0010,
      wa_j_1bbranch TYPE j_1bbranch.


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
  PARAMETERS: p_werks TYPE ckmlhd-bwkey OBLIGATORY,
              p_bdatj TYPE ckmlpp-bdatj OBLIGATORY.


  SELECT-OPTIONS: p_poper FOR ckmlpp-poper OBLIGATORY,
                  p_curtp FOR mlkey-curtp  NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT 10,
                  p_matnr FOR ckmlhd-matnr NO INTERVALS NO-EXTENSION OBLIGATORY,
                  s_matnr FOR ckmlhd-matnr OBLIGATORY NO INTERVALS.

SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: f_seleciona_dados.

  CALL SCREEN 0100.


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
  DATA: vg_last_day_aux(8),
        vg_last_day         TYPE sy-datum,
        vg_first_day_aux(8),
        vg_first_day        TYPE sy-datum,
        wl_tka02            TYPE tka02.

  s_matnr-low    = p_matnr-low.
  s_matnr-sign   = 'I'.
  s_matnr-option = 'EQ'.
  APPEND s_matnr.

  IF p_poper-low = '001'.
    vg_bdatj = p_bdatj - 1.
    vg_poper = '012'.
  ELSE.
    vg_bdatj = p_bdatj .
    vg_poper = p_poper-low - 1.
  ENDIF.
  CONCATENATE p_bdatj p_poper-low+1(2) '01' INTO vg_first_day_aux.
  vg_first_day = vg_first_day_aux.

  IF  p_poper-high IS NOT INITIAL.
    CONCATENATE p_bdatj p_poper-high+1(2) '01' INTO vg_last_day_aux.
  ELSE.
    CONCATENATE p_bdatj p_poper-low+1(2) '01' INTO vg_last_day_aux.
  ENDIF.
  vg_last_day = vg_last_day_aux.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  "somente o material principal
  SELECT *
    FROM ckmlhd
    INTO TABLE it_ckmlhd_um
    WHERE matnr IN p_matnr
    AND   bwkey EQ p_werks.

  CHECK it_ckmlhd_um IS NOT INITIAL.

  "todos os materiais de comparação
  SELECT *
      FROM ckmlhd
      INTO TABLE it_ckmlhd
      WHERE matnr IN s_matnr
      AND   bwkey EQ p_werks.

* ---> S4 Migration - 20/07/2023 - DG
*  SELECT *
*    FROM mlcd
*    INTO TABLE it_mlcd
*     FOR ALL ENTRIES IN it_ckmlhd_um
*    WHERE kalnr EQ it_ckmlhd_um-kalnr
*    AND   bdatj  EQ p_bdatj
*    AND   poper IN p_poper
*    AND   curtp EQ p_curtp-low
*    AND   categ IN ( 'ZU', 'VN' ).

  DATA: BEGIN OF gt_mlcd OCCURS 0.
          INCLUDE STRUCTURE mlcd.
  DATA:   docref TYPE mldoc-docref.
  DATA: END OF gt_mlcd.

  DATA: lv_jahrper2 TYPE mldoc-jahrper.

  CONCATENATE p_bdatj p_poper INTO lv_jahrper2.

  SELECT * FROM  mldoc
            INTO TABLE @DATA(gt_mldoc)
     FOR ALL ENTRIES IN @it_ckmlhd_um
    WHERE kalnr   EQ @it_ckmlhd_um-kalnr
    AND   jahrper EQ @lv_jahrper2
    AND   curtp  EQ @p_curtp-low
    AND   categ  IN ( 'ZU', 'VN' ).

  LOOP AT gt_mldoc INTO DATA(w_mldoc).
    MOVE-CORRESPONDING w_mldoc TO gt_mlcd.
    gt_mlcd-poper = w_mldoc-jahrper+4(3).
    gt_mlcd-bdatj = w_mldoc-jahrper(4).
    gt_mlcd-lbkum = w_mldoc-quant.
    gt_mlcd-docref = w_mldoc-docref.
    APPEND gt_mlcd.
  ENDLOOP.

  SORT gt_mldoc BY bvalt.

  " Manter select em tabela obsoleta com objetivo de buscar registros antigos que não estão na nova tabela MLDOC.
  SELECT *                             "#EC CI_DB_OPERATION_OK[2354768]
      FROM mlcd
        INTO TABLE @DATA(gt_mlcd_2)
     FOR ALL ENTRIES IN @it_ckmlhd_um
    WHERE kalnr EQ @it_ckmlhd_um-kalnr
    AND   bdatj	EQ @p_bdatj
    AND   poper IN @p_poper
    AND   curtp EQ @p_curtp-low
    AND   categ IN ( 'ZU', 'VN' ).

  LOOP AT gt_mlcd_2 ASSIGNING FIELD-SYMBOL(<fs_mlcd>).
    READ TABLE gt_mldoc WITH KEY bvalt = <fs_mlcd>-bvalt BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      APPEND <fs_mlcd> TO gt_mlcd.
    ENDIF.
  ENDLOOP.

  SORT gt_mlcd BY kalnr bdatj poper untper categ ptyp bvalt curtp docref.
  DELETE ADJACENT DUPLICATES FROM gt_mlcd COMPARING kalnr bdatj poper untper categ ptyp bvalt curtp docref.

  MOVE-CORRESPONDING gt_mlcd[] TO it_mlcd[].
* <--- S4 Migration - 20/07/2023 - DG



* ---> S4 Migration - 20/07/2023 - DG
*  SELECT *
*  FROM mlcd
*  INTO TABLE it_mlcd_tod
*   FOR ALL ENTRIES IN it_ckmlhd
*  WHERE kalnr EQ it_ckmlhd-kalnr
*  AND   bdatj  EQ p_bdatj
*  AND   poper IN p_poper
*  AND   curtp EQ p_curtp-low
*  AND   categ IN ( 'VN' )
*  AND   ptyp  EQ 'V+'.

  REFRESH: gt_mlcd, gt_mldoc, gt_mlcd_2.

  CONCATENATE p_bdatj p_poper INTO lv_jahrper2.

  SELECT * FROM  mldoc
            INTO TABLE @gt_mldoc
             FOR ALL ENTRIES IN @it_ckmlhd
               WHERE      kalnr   EQ @it_ckmlhd-kalnr
                    AND   jahrper EQ @lv_jahrper2
                    AND   curtp   EQ @p_curtp-low
                    AND   categ   IN ( 'VN' )
                    AND   ptyp    EQ 'V+'.

  LOOP AT gt_mldoc INTO w_mldoc.
    MOVE-CORRESPONDING w_mldoc TO gt_mlcd.
    gt_mlcd-poper = w_mldoc-jahrper+4(3).
    gt_mlcd-bdatj = w_mldoc-jahrper(4).
    gt_mlcd-lbkum = w_mldoc-quant.
    gt_mlcd-docref = w_mldoc-docref.
    APPEND gt_mlcd.
  ENDLOOP.

  SORT gt_mldoc BY bvalt.

  " Manter select em tabela obsoleta com objetivo de buscar registros antigos que não estão na nova tabela MLDOC.
  SELECT *                             "#EC CI_DB_OPERATION_OK[2354768]
      FROM mlcd
        INTO TABLE @gt_mlcd_2
             FOR ALL ENTRIES IN @it_ckmlhd
        WHERE kalnr EQ @it_ckmlhd-kalnr
        AND   bdatj	EQ @p_bdatj
        AND   poper IN @p_poper
        AND   curtp EQ @p_curtp-low
        AND   categ IN ( 'VN' )
        AND   ptyp  EQ 'V+'.

  LOOP AT gt_mlcd_2 ASSIGNING <fs_mlcd>.
    READ TABLE gt_mldoc WITH KEY bvalt = <fs_mlcd>-bvalt BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      APPEND <fs_mlcd> TO gt_mlcd.
    ENDIF.
  ENDLOOP.

  SORT gt_mlcd BY kalnr bdatj poper untper categ ptyp bvalt curtp docref.
  DELETE ADJACENT DUPLICATES FROM gt_mlcd COMPARING kalnr bdatj poper untper categ ptyp bvalt curtp docref.

  MOVE-CORRESPONDING gt_mlcd[] TO it_mlcd_tod[].
* <--- S4 Migration - 20/07/2023 - DG


  SELECT *
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_ckmlhd
    WHERE matnr EQ it_ckmlhd-matnr
    AND   spras EQ sy-langu.

  "
* ---> S4 Migration - 08/07/2023 - MA
*  SELECT *
*    FROM CKMLPP
*    INTO TABLE IT_CKMLPP_UM
*    FOR ALL ENTRIES IN IT_CKMLHD_UM
*    WHERE KALNR EQ  IT_CKMLHD_UM-KALNR
*    AND   BDATJ  EQ  P_BDATJ
*    AND   POPER IN  P_POPER.


  DATA: wa_kalnr  TYPE ckmv0_matobj_str,
        lt_kalnr  TYPE ckmv0_matobj_tbl,
        lt_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE,
        lt_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE.

  DATA: lv_bdatj_1 TYPE  ckmlpp-bdatj,
        lv_poper_1 TYPE  ckmlpp-poper,
        lv_jahrper TYPE mldoc-jahrper.
  TYPES lr_poper_type TYPE RANGE OF poper.

  DATA : lr_poper TYPE lr_poper_type.

  lr_poper = VALUE lr_poper_type( LET s = 'I'
                                o = 'BT'
                            IN sign   = s
                               option = o
                               ( low = p_poper ) ).

  lv_bdatj_1 =  p_bdatj.


  LOOP AT it_ckmlhd_um INTO DATA(wa_ckmlhd_um).
    wa_kalnr-kalnr = wa_ckmlhd_um-kalnr.
    APPEND wa_kalnr TO lt_kalnr.
  ENDLOOP.

  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
    EXPORTING
      i_bdatj_1               = lv_bdatj_1
*     i_poper_1               = lv_poper_1
    TABLES
      t_kalnr                 = lt_kalnr
      t_ckmlpp                = lt_ckmlpp
*     t_ckmlcr                = lt_ckmlcr
    EXCEPTIONS
      no_data_found           = 1
      input_data_inconsistent = 2
      buffer_inconsistent     = 3
      OTHERS                  = 4.

  IF lt_ckmlpp[] IS NOT INITIAL.

    DELETE lt_ckmlpp WHERE  poper IN  lr_poper.

    IF sy-subrc = 0 AND lines( lt_ckmlpp[] ) > 0.

      MOVE-CORRESPONDING lt_ckmlpp[] TO it_ckmlpp_um[].
      sy-dbcnt = lines( lt_ckmlpp[] ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.
  ENDIF.
* <--- S4 Migration - 08/07/2023 - MA

* ---> S4 Migration - 08/07/2023 - MA
*  SELECT *
*      FROM ckmlpp
*      INTO TABLE it_ckmlpp
*      FOR ALL ENTRIES IN it_ckmlhd
*      WHERE kalnr EQ  it_ckmlhd-kalnr
*      AND   bdatj  EQ  p_bdatj
*      AND   poper IN  p_poper.

  CLEAR lr_poper.

  lr_poper = VALUE lr_poper_type( LET s = 'I'
                                      o = 'BT'
                              IN sign   = s
                                 option = o
                                  ( low = p_poper ) ).

  lv_bdatj_1 =  p_bdatj.

  REFRESH lt_ckmlpp.

  LOOP AT it_ckmlhd INTO DATA(wa_ckmlhd).
    wa_kalnr-kalnr = wa_ckmlhd-kalnr.
    APPEND wa_kalnr TO lt_kalnr.
  ENDLOOP.

  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
    EXPORTING
      i_bdatj_1               = lv_bdatj_1
*     i_poper_1               = lv_poper_1
    TABLES
      t_kalnr                 = lt_kalnr
      t_ckmlpp                = lt_ckmlpp
*     t_ckmlcr                = lt_ckmlcr
    EXCEPTIONS
      no_data_found           = 1
      input_data_inconsistent = 2
      buffer_inconsistent     = 3
      OTHERS                  = 4.

  IF lt_ckmlpp[] IS NOT INITIAL.

    DELETE lt_ckmlpp WHERE poper NOT IN   lr_poper.

    IF sy-subrc = 0 AND lines( lt_ckmlpp[] ) > 0.

      MOVE-CORRESPONDING lt_ckmlpp[] TO it_ckmlpp[].
      sy-dbcnt = lines( lt_ckmlpp[] ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.
  ENDIF.
* <--- S4 Migration - 08/07/2023 - MA
* ---> S4 Migration - 08/07/2023 - MA
**  SELECT *
**    FROM ckmlcr
**    INTO TABLE it_ckmlcr
**    FOR ALL ENTRIES IN it_ckmlhd_um
**    WHERE kalnr EQ it_ckmlhd_um-kalnr
**    AND   bdatj	EQ p_bdatj
**    AND   poper IN p_poper
**    AND   curtp EQ p_curtp-low.


  CLEAR lr_poper.

  lr_poper = VALUE lr_poper_type( LET s = 'I'
                                      o = 'BT'
                              IN sign   = s
                                 option = o
                                  ( low = p_poper ) ).

  lv_bdatj_1 =  p_bdatj.

  REFRESH: lt_ckmlcr, lt_kalnr.

  LOOP AT it_ckmlhd_um INTO wa_ckmlhd_um.
    wa_kalnr-kalnr = wa_ckmlhd_um-kalnr.
    APPEND wa_kalnr TO lt_kalnr.
  ENDLOOP.

  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
    EXPORTING
      i_bdatj_1               = lv_bdatj_1
*     i_poper_1               = lv_poper_1
    TABLES
      t_kalnr                 = lt_kalnr
*     t_ckmlpp                = lt_ckmlpp
      t_ckmlcr                = lt_ckmlcr
    EXCEPTIONS
      no_data_found           = 1
      input_data_inconsistent = 2
      buffer_inconsistent     = 3
      OTHERS                  = 4.

  IF lt_ckmlpp[] IS NOT INITIAL.

    DELETE lt_ckmlcr WHERE poper NOT IN lr_poper AND curtp NE p_curtp-low.

    IF sy-subrc = 0 AND lines( lt_ckmlpp[] ) > 0.

      MOVE-CORRESPONDING lt_ckmlcr[] TO it_ckmlcr[].
      sy-dbcnt = lines( lt_ckmlcr[] ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.
  ENDIF.
* <--- S4 Migration - 08/07/2023 - MA

  "MES ANTERIOR
* ---> S4 Migration - 08/07/2023 - MA
*  SELECT *
*    FROM ckmlcr
*    APPENDING TABLE it_ckmlcr
*    FOR ALL ENTRIES IN it_ckmlhd_um
*    WHERE kalnr EQ it_ckmlhd_um-kalnr
*    AND   bdatj  EQ vg_bdatj
*    AND   poper EQ vg_poper
*    AND   curtp EQ p_curtp-low.


  lv_bdatj_1 =  vg_bdatj.
  lv_poper_1 = vg_poper.

  REFRESH: lt_ckmlcr, lt_kalnr.

  LOOP AT it_ckmlhd_um INTO wa_ckmlhd_um.
    wa_kalnr-kalnr = wa_ckmlhd_um-kalnr.
    APPEND wa_kalnr TO lt_kalnr.
  ENDLOOP.

  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
    EXPORTING
      i_bdatj_1               = lv_bdatj_1
      i_poper_1               = lv_poper_1
    TABLES
      t_kalnr                 = lt_kalnr
*     t_ckmlpp                = lt_ckmlpp
      t_ckmlcr                = lt_ckmlcr
    EXCEPTIONS
      no_data_found           = 1
      input_data_inconsistent = 2
      buffer_inconsistent     = 3
      OTHERS                  = 4.

  IF lt_ckmlpp[] IS NOT INITIAL.

    DELETE lt_ckmlcr WHERE curtp NE p_curtp-low.

    IF sy-subrc = 0 AND lines( lt_ckmlcr[] ) > 0.

      MOVE-CORRESPONDING lt_ckmlcr[] TO it_ckmlcr[].
      sy-dbcnt = lines( lt_ckmlcr[] ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.
  ENDIF.
* <--- S4 Migration - 08/07/2023 - MA

  IF p_poper-high IS NOT INITIAL.
    DELETE it_ckmlcr WHERE bdatj  EQ p_bdatj AND poper = p_poper-high. "ELIMINA ULTIMO MES
  ELSE.
    DELETE it_ckmlcr WHERE bdatj  EQ p_bdatj AND poper = p_poper-low. "ELIMINA ULTIMO MES
  ENDIF.

  SELECT SINGLE *
    FROM j_1bbranch
    INTO wa_j_1bbranch
    WHERE branch = p_werks.

  SELECT SINGLE *
          FROM tka02
          INTO wl_tka02
          WHERE bukrs  = wa_j_1bbranch-bukrs.

  SELECT *
    FROM zcot0010
    INTO TABLE it_zcot0010.

  CHECK it_zcot0010[] IS NOT INITIAL.

  SELECT *
   FROM bsis
   INNER JOIN csks
   ON   kokrs      EQ wl_tka02-kokrs
   AND  csks~kostl EQ bsis~kostl
   AND  csks~datab LE sy-datum
   AND  csks~datbi GE sy-datum
   AND  csks~bukrs EQ wa_j_1bbranch-bukrs
   AND  csks~kosar EQ 'F'
   INTO CORRESPONDING FIELDS OF TABLE it_bsis
   FOR ALL ENTRIES IN it_zcot0010
   WHERE bsis~bukrs  EQ  wa_j_1bbranch-bukrs
   AND   bsis~hkont  EQ  it_zcot0010-saknr
   AND   bsis~gjahr  EQ  p_bdatj
   AND   bsis~budat  BETWEEN vg_first_day AND vg_last_day
   AND   bsis~gsber  EQ  p_werks.


  SELECT *
    FROM skat
    INTO TABLE it_skat
    FOR ALL ENTRIES IN it_zcot0010
    WHERE saknr EQ it_zcot0010-saknr
    AND spras EQ sy-langu
    AND ktopl EQ '0050'.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
FORM f_saida.
  SORT: it_ckmlhd BY matnr,
      it_ckmlpp BY kalnr bdatj poper,
      it_ckmlcr BY kalnr bdatj poper,
      it_mlcd   BY kalnr bdatj poper,
      it_mlcd_tod BY kalnr bdatj poper,
      it_makt   BY matnr,
      it_skat   BY saknr.

  DATA: v_valor(10),
        v_total(10),
        v_perce(10),
        v_mes(02),
        v_cod_nat(2),
        tabix               TYPE sy-tabix.

  DATA: refe1	     TYPE zwrbtr_dre,
        vmes       TYPE monat,
        vnivel(10).

  FIELD-SYMBOLS: <fs_val> TYPE any,
                 <fs_tot> TYPE any,
                 <fs_prd> TYPE any,
                 <fs_per> TYPE any,
                 <fs_abe> TYPE any.

  "estoque quantidade
  CLEAR wa_saida.
  wa_saida-niv00  = 'X'.
  wa_saida-nivel  = '01'.
  wa_saida-nitxt  = 'QUANTIDADE EM ESTOQUE (CKM3N)'.
  APPEND wa_saida TO gt_saida.

  CLEAR: wa_saida, wa_saida_prd, wa_saida_con, wa_saida_acu, wa_saida_fin.
  LOOP AT it_ckmlpp_um INTO wa_ckmlpp.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_ckmlpp-poper
      IMPORTING
        output = v_mes.

    CONCATENATE 'VLR_' v_mes INTO v_valor.
    ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida TO <fs_val>.
    ADD wa_ckmlpp-abkumo    TO <fs_val>.

    ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_prd TO <fs_val>.
    ADD wa_ckmlpp-zukumo     TO <fs_val>.

    ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_acu TO <fs_val>.
    ADD wa_ckmlpp-zukumo    TO <fs_val>.
    ADD wa_ckmlpp-abkumo    TO <fs_val>.

*    ASSIGN COMPONENT V_VALOR OF STRUCTURE WA_SAIDA_CON TO <FS_VAL>.
*    ADD WA_CKMLPP-VNKUMO     TO <FS_VAL>.

  ENDLOOP.
  "consumo
  CLEAR: wa_saida_con, wa_saida_tra.
  LOOP AT it_mlcd INTO wa_mlcd.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_mlcd-poper
      IMPORTING
        output = v_mes.

    CONCATENATE 'VLR_' v_mes INTO v_valor.

    IF wa_mlcd-ptyp = 'V+'.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_con TO <fs_val>.
    ELSEIF wa_mlcd-ptyp = 'VU'.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_tra TO <fs_val>.
    ELSE.
      CONTINUE.
    ENDIF.
    <fs_val> =  <fs_val> + ( wa_mlcd-lbkum ).
  ENDLOOP.

  wa_saida-nivel  = '01.01'.
  wa_saida-nitxt  = 'Estoque Inicial'.
  APPEND wa_saida TO gt_saida.

  wa_saida_prd-nivel  = '01.02'.
  wa_saida_prd-nitxt  = '(+) Entrada Produção'.
  APPEND wa_saida_prd TO gt_saida.

  wa_saida_acu-nivel  = '01.03'.
  wa_saida_acu-nitxt  = 'Estoque Acumulado'.
  wa_saida_acu-niv00  = '+'.
  APPEND wa_saida_acu TO gt_saida.

  wa_saida_con-nivel  = '01.04'.
  wa_saida_con-nitxt  = '(-) Consumo (Vendas)'.
  wa_saida_con-niv00  = '-'.
  APPEND wa_saida_con TO gt_saida.

  wa_saida_tra-nivel  = '01.05'.
  wa_saida_tra-nitxt  = '(-) Transferências entre centros'.
  wa_saida_tra-niv00  = '-'.
  APPEND wa_saida_tra TO gt_saida.

  "VALORES
  "estoque INICIAL
  CLEAR wa_saida.
  wa_saida-niv00  = 'X'.
  wa_saida-nivel  = '02'.
  wa_saida-nitxt  = 'CUSTO EM ESTOQUE (CKM3N)'.
  APPEND wa_saida TO gt_saida.

  CLEAR: wa_saida, wa_saida_prd, wa_saida_con, wa_saida_acu, wa_saida_fin.
  LOOP AT it_ckmlcr INTO wa_ckmlcr.
    IF wa_ckmlcr-poper = '012'.
      vg_poper = '001'.
    ELSE.
      vg_poper = wa_ckmlcr-poper + 1.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vg_poper
      IMPORTING
        output = v_mes.

    CONCATENATE 'VLR_' v_mes INTO v_valor.
    ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida TO <fs_val>.
    ADD wa_ckmlcr-salk3     TO <fs_val>.
  ENDLOOP.
  wa_saida-nivel  = '02.01'.
  wa_saida-nitxt  = 'Estoque Inicial'.
  wa_saida-niv00  = '+'.
  APPEND wa_saida TO gt_saida.

  "+) Entradas (Produção) / (-) Consumo (Vendas)
  CLEAR: wa_saida, wa_saida_prd, wa_saida_con, wa_saida_acu,wa_saida_tra, wa_saida_fin.
  LOOP AT it_mlcd INTO wa_mlcd.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_mlcd-poper
      IMPORTING
        output = v_mes.

    CONCATENATE 'VLR_' v_mes INTO v_valor.
    IF wa_mlcd-categ = 'ZU'.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_prd TO <fs_val>.
    ELSEIF wa_mlcd-ptyp = 'V+'.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_con TO <fs_val>.
    ELSEIF wa_mlcd-ptyp = 'VU'.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_tra TO <fs_val>.
    ELSE.
      CONTINUE.
    ENDIF.
    <fs_val> =  <fs_val> + ( wa_mlcd-salk3 + wa_mlcd-estprd + wa_mlcd-estkdm + wa_mlcd-mstprd +  wa_mlcd-mstkdm ).
  ENDLOOP.


  wa_saida_prd-nivel  = '02.02'.
  wa_saida_prd-nitxt  = '(+) Entradas (Produção)'.
  wa_saida_prd-niv00  = '+'.
  APPEND wa_saida_prd TO gt_saida.

  wa_saida_con-nivel  = '02.03'.
  wa_saida_con-nitxt  = '(-) Consumo (Vendas)'.
  wa_saida_con-niv00  = '-'.
  APPEND wa_saida_con TO gt_saida.

  wa_saida_tra-nivel  = '02.04'.
  wa_saida_tra-nitxt  = '(-) Transferências entre centros'.
  wa_saida_tra-niv00  = '-'.
  APPEND wa_saida_tra TO gt_saida.

  "Produção + estoque anterior
  CLEAR wa_saida.
  wa_saida-niv00  = 'X'.
  wa_saida-nivel  = '03'.
  wa_saida-nitxt  = 'PRODUÇÃO + ESTOQUE ANTERIOR (CKM3N)'.
  APPEND wa_saida TO gt_saida.
  CLEAR: wa_saida_acu.
  LOOP AT it_ckmlhd INTO wa_ckmlhd.
    CLEAR: wa_saida.
    LOOP AT it_ckmlpp INTO wa_ckmlpp WHERE  kalnr  =  wa_ckmlhd-kalnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ckmlpp-poper
        IMPORTING
          output = v_mes.

      CONCATENATE 'VLR_' v_mes INTO v_valor.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida TO <fs_val>.
      ADD wa_ckmlpp-zukumo    TO <fs_val>.
      ADD wa_ckmlpp-abkumo    TO <fs_val>.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_acu TO <fs_val>.
      ADD wa_ckmlpp-zukumo    TO <fs_val>.
      ADD wa_ckmlpp-abkumo    TO <fs_val>.
    ENDLOOP.
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ckmlhd-matnr BINARY SEARCH.
    wa_saida-nivel  = '03'.
    wa_saida-nitxt  = 'PRODUÇÃO + ESTOQUE ANTERIOR (CKM3N)'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_ckmlhd-matnr
      IMPORTING
        output = wa_saida-matnr.

    wa_saida-maktx  = wa_makt-maktx.
    wa_saida-niv00  = '+'.
    APPEND wa_saida TO gt_saida.
  ENDLOOP.

  CLEAR wa_saida.
  wa_saida-niv00  = 'X'.
  wa_saida-nivel  = '04'.
  wa_saida-nitxt  = '% DISTRIBUIÇÃO DA PRODUÇÃO'.
  APPEND wa_saida TO gt_saida.

  LOOP AT it_ckmlhd INTO wa_ckmlhd.
    CLEAR: wa_saida.
    LOOP AT it_ckmlpp INTO wa_ckmlpp WHERE  kalnr  =  wa_ckmlhd-kalnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ckmlpp-poper
        IMPORTING
          output = v_mes.

      CONCATENATE 'VLR_' v_mes INTO v_valor.
      CONCATENATE 'VLR_' v_mes INTO v_total.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida TO <fs_val>.
      ADD wa_ckmlpp-zukumo    TO <fs_val>.
      ADD wa_ckmlpp-abkumo    TO <fs_val>.

      ASSIGN COMPONENT v_total OF STRUCTURE wa_saida_acu TO <fs_tot>.

      IF <fs_tot> NE 0.
        <fs_val> = ( <fs_val> / <fs_tot> ) * 100.
      ENDIF.

    ENDLOOP.
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ckmlhd-matnr BINARY SEARCH.
    wa_saida-nivel  = '04'.
    wa_saida-nitxt  = '% DISTRIBUIÇÃO DA PRODUÇÃO'.
    wa_saida-niv00  = '+'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_ckmlhd-matnr
      IMPORTING
        output = wa_saida-matnr.
    wa_saida-maktx  = wa_makt-maktx.
    APPEND wa_saida TO gt_saida.
  ENDLOOP.

  CLEAR wa_saida.
  wa_saida-niv00  = 'X'.
  wa_saida-nivel  = '05'.
  wa_saida-nitxt  = 'VENDAS (SAÍDAS)'.
  APPEND wa_saida TO gt_saida.

  CLEAR: wa_saida_acu.
  LOOP AT it_ckmlhd INTO wa_ckmlhd.
    CLEAR: wa_saida.
    LOOP AT it_mlcd_tod INTO wa_mlcd WHERE  kalnr  =  wa_ckmlhd-kalnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_mlcd-poper
        IMPORTING
          output = v_mes.

      CONCATENATE 'VLR_' v_mes INTO v_valor.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida TO <fs_val>.
      <fs_val> =  <fs_val> + ( wa_mlcd-lbkum ).
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_acu TO <fs_val>.
      <fs_val> =  <fs_val> + ( wa_mlcd-lbkum ).
    ENDLOOP.
*    LOOP AT IT_CKMLPP INTO WA_CKMLPP WHERE  KALNR  =  WA_CKMLHD-KALNR.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_CKMLPP-POPER
*        IMPORTING
*          OUTPUT = V_MES.
*
*      CONCATENATE 'VLR_' V_MES INTO V_VALOR.
*      ASSIGN COMPONENT V_VALOR OF STRUCTURE WA_SAIDA TO <FS_VAL>.
*      ADD WA_CKMLPP-VNKUMO    TO <FS_VAL>.
*      ASSIGN COMPONENT V_VALOR OF STRUCTURE WA_SAIDA_ACU TO <FS_VAL>.
*      ADD WA_CKMLPP-VNKUMO    TO <FS_VAL>.
*    ENDLOOP.
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ckmlhd-matnr BINARY SEARCH.
    wa_saida-nivel  = '05'.
    wa_saida-nitxt  = 'VENDAS (SAÍDAS)'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_ckmlhd-matnr
      IMPORTING
        output = wa_saida-matnr.
    wa_saida-maktx  = wa_makt-maktx.
    wa_saida-niv00  = '+'.
    APPEND wa_saida TO gt_saida.
  ENDLOOP.

  CLEAR wa_saida.
  wa_saida-niv00  = 'X'.
  wa_saida-nivel  = '06'.
  wa_saida-nitxt  = ' % DISTRIBUIÇÃO DAS VENDAS'.
  APPEND wa_saida TO gt_saida.

  LOOP AT it_ckmlhd INTO wa_ckmlhd.
    CLEAR: wa_saida.
    LOOP AT it_mlcd_tod INTO wa_mlcd WHERE  kalnr  =  wa_ckmlhd-kalnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_mlcd-poper
        IMPORTING
          output = v_mes.

      CONCATENATE 'VLR_' v_mes INTO v_valor.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida TO <fs_val>.
      <fs_val> =  <fs_val> + ( wa_mlcd-lbkum ).

    ENDLOOP.
    LOOP AT it_ckmlpp INTO wa_ckmlpp WHERE  kalnr  =  wa_ckmlhd-kalnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ckmlpp-poper
        IMPORTING
          output = v_mes.

      CONCATENATE 'VLR_' v_mes INTO v_valor.
      CONCATENATE 'VLR_' v_mes INTO v_total.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida TO <fs_val>.

      ASSIGN COMPONENT v_total OF STRUCTURE wa_saida_acu TO <fs_tot>.

      IF <fs_tot> NE 0.
        <fs_val> = ( <fs_val> / <fs_tot> ) * 100.
      ENDIF.

    ENDLOOP.
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ckmlhd-matnr BINARY SEARCH.
    wa_saida-nivel  = '06'.
    wa_saida-nitxt  = ' % DISTRIBUIÇÃO DAS VENDAS'.
    wa_saida-niv00  = '+'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_ckmlhd-matnr
      IMPORTING
        output = wa_saida-matnr.
    wa_saida-maktx  = wa_makt-maktx.
    APPEND wa_saida TO gt_saida.
  ENDLOOP.

  " Despesas
  CLEAR: wa_saida, wa_saida_abe.
  SORT it_zcot0010 BY cod_nat saknr.
  LOOP AT it_zcot0010 INTO wa_zcot0010.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zcot0010-cod_nat
      IMPORTING
        output = v_cod_nat.
    wa_saida_abe-nivel  = v_cod_nat.
    wa_saida_abe-nitxt  = wa_zcot0010-des_nat.
    wa_saida_abe-niv00  = '+'.
    wa_saida_abe-saknr  =  wa_zcot0010-saknr.
    READ TABLE it_skat INTO wa_skat WITH KEY saknr =  wa_zcot0010-saknr BINARY SEARCH.
    wa_saida_abe-maktx  = wa_skat-txt50.
    APPEND wa_saida_abe TO gt_saida_abe.
  ENDLOOP.

  SORT: gt_saida_abe BY saknr.
  LOOP AT it_bsis INTO wa_bsis.
    CONCATENATE 'VLR_' wa_bsis-budat+4(2) INTO v_valor.

    ASSIGN COMPONENT v_valor OF STRUCTURE     wa_saida TO <fs_val>.

    IF wa_bsis-shkzg = 'H'.
      SUBTRACT wa_bsis-dmbtr    FROM <fs_val>.
    ELSE.
      ADD wa_bsis-dmbtr    TO <fs_val>.
    ENDIF.
    "Abertura da conta
    READ TABLE gt_saida_abe INTO wa_saida_abe WITH KEY saknr = wa_bsis-hkont BINARY SEARCH.
    IF sy-subrc = 0.
      tabix = sy-tabix.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_abe TO <fs_abe>.
      IF wa_bsis-shkzg = 'H'.
        SUBTRACT wa_bsis-dmbtr    FROM <fs_abe>.
      ELSE.
        ADD wa_bsis-dmbtr    TO <fs_abe>.
      ENDIF.
      MODIFY gt_saida_abe FROM wa_saida_abe INDEX tabix.
    ENDIF.
  ENDLOOP.
  SORT  gt_saida_abe BY nivel saknr.

  wa_saida-nivel  = '07'.
  wa_saida-nitxt  = 'DESPESAS NO MÊS'.
  wa_saida-niv00  = '+'.
  APPEND wa_saida TO gt_saida.

  LOOP AT gt_saida_abe INTO wa_saida_abe.
    wa_saida-nivel = wa_saida_abe-nivel.
    CONCATENATE '07.' wa_saida_abe-nivel  INTO wa_saida_abe-nivel.
    IF vnivel NE wa_saida-nivel.
      MOVE-CORRESPONDING wa_saida_abe TO wa_saida.
      CLEAR: wa_saida-saknr,wa_saida-vlr_01,wa_saida-vlr_02,wa_saida-vlr_03,wa_saida-vlr_04,wa_saida-vlr_05,wa_saida-vlr_06,wa_saida-vlr_07,wa_saida-vlr_08,wa_saida-vlr_09,wa_saida-vlr_10,wa_saida-vlr_11,wa_saida-vlr_12.
      wa_saida-niv00  = 'S'.
      APPEND wa_saida TO gt_saida.
      vnivel = wa_saida_abe-nivel+3(2).
    ENDIF.
    wa_saida-nivel = wa_saida_abe-nivel.

    APPEND wa_saida_abe TO gt_saida.
  ENDLOOP.

  CLEAR wa_saida.
  wa_saida-niv00  = 'X'.
  wa_saida-nivel  = '08'.
  wa_saida-nitxt  = 'DISTRIBUIÇÃO GASTOS'.
  APPEND wa_saida TO gt_saida.

  LOOP AT it_ckmlhd INTO wa_ckmlhd.
    CLEAR: wa_saida_acu,wa_saida_con,wa_saida, wa_saida_prd.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_ckmlhd-matnr
      IMPORTING
        output = wa_saida_prd-matnr.
    READ TABLE gt_saida INTO wa_saida_acu WITH KEY nivel = '04'
                                                   matnr = wa_saida_prd-matnr.
    READ TABLE gt_saida INTO wa_saida_con WITH KEY nivel = '07'.

    LOOP AT it_ckmlpp INTO wa_ckmlpp WHERE  kalnr  =  wa_ckmlhd-kalnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ckmlpp-poper
        IMPORTING
          output = v_mes.

      CONCATENATE 'VLR_' v_mes INTO v_valor.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida     TO <fs_val>. "
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_acu TO <fs_per>. "% distribuicao produção
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_con TO <fs_tot>. "total despesas

      <fs_val> = ( <fs_per> * <fs_tot> ) / 100.

    ENDLOOP.
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ckmlhd-matnr BINARY SEARCH.
    wa_saida-nivel  = '08'.
    wa_saida-nitxt  = 'DISTRIBUIÇÃO GASTOS'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_ckmlhd-matnr
      IMPORTING
        output = wa_saida-matnr.
    wa_saida-maktx  = wa_makt-maktx.
    wa_saida-niv00  = '+'.
    APPEND wa_saida TO gt_saida.
  ENDLOOP.

**********************************************************************************************************************
  CLEAR wa_saida.
  wa_saida-niv00  = 'X'.
  wa_saida-nivel  = '09'.
  wa_saida-nitxt  = 'DEMONSTRATIVO DO CUSTO'.
  APPEND wa_saida TO gt_saida.

  LOOP AT it_ckmlhd_um INTO wa_ckmlhd.
    CLEAR:  wa_saida_a,
            wa_saida_b,
            wa_saida_c,
            wa_saida_d,
            wa_saida_f,
            wa_saida_g,
            wa_saida_h,
            wa_saida_i,
            wa_saida_j,
            wa_saida_l,
            wa_saida_m.

    CLEAR:  wa_saida_a1,
            wa_saida_b1,
            wa_saida_c1,
            wa_saida_d1,
            wa_saida_f1,
            wa_saida_g1,
            wa_saida_h1,
            wa_saida_i1,
            wa_saida_j1,
            wa_saida_l1,
            wa_saida_m1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_ckmlhd-matnr
      IMPORTING
        output = wa_saida_prd-matnr.

    READ TABLE gt_saida INTO wa_saida_b1 WITH KEY nivel = '02.02'. "Entrada produção (Valor)
    READ TABLE gt_saida INTO wa_saida_a1 WITH KEY nivel = '02.01'. "Estoque inicial (Valor)

    READ TABLE gt_saida INTO wa_saida_c1 WITH KEY nivel = '01.02'. "Entrada produção (Qtde)
    READ TABLE gt_saida INTO wa_saida_d1 WITH KEY nivel = '01.01'. "Estoque inicial (Qtde)

    READ TABLE gt_saida INTO wa_saida_e1 WITH KEY nivel = '08'  "DISTRIBUIÇÃO GASTOS
                                                  matnr = wa_saida_prd-matnr.

    READ TABLE gt_saida INTO wa_saida_f1 WITH KEY nivel = '05'  "VENDAS (SAÍDAS)
                                                  matnr = wa_saida_prd-matnr.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ckmlhd-matnr BINARY SEARCH.

    CLEAR wa_saida.
    LOOP AT it_ckmlpp_um INTO wa_ckmlpp WHERE  kalnr  =  wa_ckmlhd-kalnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ckmlpp-poper
        IMPORTING
          output = v_mes.

      CONCATENATE 'VLR_' v_mes INTO v_valor.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_a1 TO <fs_val>. "estoque inicial
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_b1 TO <fs_prd>. "Entrada produção
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_a  TO <fs_tot>. "Custo Produção

      IF <fs_prd> = 0.
        <fs_tot> = 0.
      ELSE.
        <fs_tot> = <fs_val> + <fs_prd>.
      ENDIF.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_d1 TO <fs_val>. "estoque inicial
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_c1 TO <fs_prd>. "Entrada produção (Qtde)
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_b  TO <fs_tot>. "Qtd Produzida + Est final mês anterior

      IF <fs_prd> = 0.
        <fs_tot> = 0.
      ELSE.
        <fs_tot> = <fs_val> + <fs_prd>.
      ENDIF.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_f1  TO <fs_tot>. "Qtd vendida no mês
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_d   TO <fs_val>. "Qtd vendida no mês
      ADD <fs_tot> TO <fs_val>.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_b  TO <fs_prd>. "Qtd Produzida + Est final mês anterior
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_m  TO <fs_per>. "% Vendido da produção
      IF <fs_prd> GT 0.
        <fs_per> = ( <fs_val> / <fs_prd> ) * 100.
      ELSE.
        <fs_per> = 0.
      ENDIF.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_c  TO <fs_prd>. "Custo s/ Venda MP
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_a  TO <fs_tot>. "Custo Produção
      <fs_prd> = ( <fs_per> / 100 ) * <fs_tot>.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_e1  TO <fs_tot>. "DISTRIBUIÇÃO GASTOS
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_e   TO <fs_val>. "Despesas Operacionais
      ADD <fs_tot> TO <fs_val>.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_a  TO <fs_per>. "Custo Produção
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_f  TO <fs_tot>. "Custos Produção - Despesas Operacionais
      <fs_tot> = <fs_per> - <fs_val>.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_g  TO <fs_prd>. "Custo MP s/ % Vendido
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_m  TO <fs_per>. "% Vendido da produção
      <fs_prd> = <fs_tot> * ( <fs_per> / 100 ).

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_h  TO <fs_tot>. "Despesas s/ % Vendido
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_e  TO <fs_val>. "Despesas Operacionais
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_m  TO <fs_per>. "% Vendido da produção
      <fs_tot> = <fs_val> * ( <fs_per> / 100 ).

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_i  TO <fs_tot>. "Custo do Produto Vendido no Mês
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_g  TO <fs_prd>. "Custo MP s/ % Vendido
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_h  TO <fs_per>. "Despesas s/ % Vendido
      <fs_tot> =  <fs_prd> +  <fs_per>.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_j  TO <fs_tot>. "Saldo Despesas não apropriadas
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_e  TO <fs_val>. "Despesas Operacionais
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_h  TO <fs_per>. "Despesas s/ % Vendido
      <fs_tot> =  <fs_val> -  <fs_per>.

      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_k  TO <fs_tot>. "Saldo Custo MP não apropriado
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_a  TO <fs_per>. "Custo Produção
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_c  TO <fs_prd>. "Custo s/ Venda MP
      IF <fs_per> GT 0.
        <fs_tot> =  <fs_per> -  <fs_prd>.
      ENDIF.

      READ TABLE gt_saida INTO wa_saida_l1 WITH KEY nivel = '04'  "% Despesas Produção Total (Absorção)
                                                  matnr = wa_saida_prd-matnr.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_l  TO <fs_tot>.
      ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_l1  TO <fs_val>.
      <fs_tot> = <fs_val>.

    ENDLOOP.


    wa_saida_a-nivel  = '09.01'.
    wa_saida_a-nitxt  = 'Custo Produção'.
    wa_saida_a-niv00  = '+'.
    APPEND wa_saida_a TO gt_saida.

    wa_saida_b-nivel  = '09.02'.
    wa_saida_b-nitxt  = 'Qtd Produzida + Est final mês anterior'.
    wa_saida_b-niv00  = '+'.
    APPEND wa_saida_b TO gt_saida.

    wa_saida_c-nivel  = '09.03'.
    wa_saida_c-nitxt  = 'Custo s/ Venda MP'.
    wa_saida_c-niv00  = '+'.
    APPEND wa_saida_c TO gt_saida.


    wa_saida_d-nivel  = '09.04'.
    wa_saida_d-nitxt  = 'Qtd vendida no mês'.
    wa_saida_d-niv00  = '+'.
    APPEND wa_saida_d TO gt_saida.

    wa_saida_e-nivel  = '09.05'.
    wa_saida_e-niv00  = '+'.
    wa_saida_e-nitxt  = 'Despesas Operacionais'.
    APPEND wa_saida_e TO gt_saida.

    wa_saida_f-nivel  = '09.06'.
    wa_saida_f-nitxt  = 'Custos Produção - Despesas Operacionais'.
    wa_saida_f-niv00  = '+'.
    APPEND wa_saida_f TO gt_saida.

    wa_saida_g-nivel  = '09.07'.
    wa_saida_g-nitxt  = 'Custo MP s/ % Vendido'.
    wa_saida_g-niv00  = '+'.
    APPEND wa_saida_g TO gt_saida.

    wa_saida_h-nivel  = '09.08'.
    wa_saida_h-nitxt  = 'Despesas s/ % Vendido'.
    wa_saida_h-niv00  = '+'.
    APPEND wa_saida_h TO gt_saida.

    wa_saida_i-nivel  = '09.09'.
    wa_saida_i-nitxt  = 'Custo do Produto Vendido no Mês'.
    wa_saida_i-niv00  = '+'.
    APPEND wa_saida_i TO gt_saida.

    wa_saida_j-nivel  = '09.10'.
    wa_saida_j-nitxt  = 'Saldo Despesas não apropriadas'.
    wa_saida_j-niv00  = '+'.
    APPEND wa_saida_j TO gt_saida.

    wa_saida_k-nivel  = '09.11'.
    wa_saida_k-nitxt  = 'Saldo Custo MP não apropriado'.
    wa_saida_k-niv00  = '+'.
    APPEND wa_saida_k TO gt_saida.

    wa_saida_l-nivel  = '09.12'.
    wa_saida_l-nitxt  = '% Despesas Produção Total (Absorção)'.
    wa_saida_l-niv00  = '+'.
    APPEND wa_saida_l TO gt_saida.

    CLEAR wa_saida.
    wa_saida-nivel  = '09.13'.
    wa_saida-nitxt  = '$ Despesas Produção Total (Absorção)'.
    wa_saida-niv00  = 'T'.
    APPEND wa_saida TO gt_saida.

    CLEAR vnivel.
    LOOP AT gt_saida_abe INTO wa_saida_abe.
      wa_saida-nivel = wa_saida_abe-nivel.
      CONCATENATE '09.13.' wa_saida_abe-nivel  INTO wa_saida_abe-nivel.
      IF vnivel NE wa_saida-nivel.
        MOVE-CORRESPONDING wa_saida_abe TO wa_saida.
        CLEAR: wa_saida-saknr,wa_saida-vlr_01,wa_saida-vlr_02,wa_saida-vlr_03,wa_saida-vlr_04,wa_saida-vlr_05,wa_saida-vlr_06,wa_saida-vlr_07,wa_saida-vlr_08,wa_saida-vlr_09,wa_saida-vlr_10,wa_saida-vlr_11,wa_saida-vlr_12.
        wa_saida-niv00  = 'S'.
        APPEND wa_saida TO gt_saida.
        vnivel = wa_saida_abe-nivel+6(2).
      ENDIF.
      vmes = 1.
      DO vmes TIMES VARYING refe1 FROM wa_saida_l-vlr_01 NEXT wa_saida_l-vlr_02.
        v_mes = sy-index.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_mes
          IMPORTING
            output = v_mes.
        CONCATENATE 'VLR_' v_mes INTO v_valor.
        ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_abe TO <fs_val>.
        <fs_val> = <fs_val> * ( refe1 / 100 ).
      ENDDO.
      APPEND wa_saida_abe TO gt_saida.
    ENDLOOP.

    wa_saida_m-nivel  = '09.14'.
    wa_saida_m-nitxt  = '% Vendido da produção'.
    wa_saida_m-niv00  = '+'.
    APPEND wa_saida_m TO gt_saida.


    CLEAR wa_saida.
    wa_saida-nivel  = '09.15'.
    wa_saida-nitxt  = '$ Vendido da produção'.
    wa_saida-niv00  = 'T'.
    APPEND wa_saida TO gt_saida.

    CLEAR vnivel.
    gt_saida_abe[] = gt_saida[].
    DELETE gt_saida_abe WHERE nivel+0(6) NE '09.13.'.

    LOOP AT gt_saida_abe INTO wa_saida_abe.
      CONCATENATE '09.15.' wa_saida_abe-nivel+6(2)  INTO wa_saida_abe-nivel.
      IF wa_saida_abe-saknr IS NOT INITIAL.
        vmes = 1.
        DO vmes TIMES VARYING refe1 FROM wa_saida_m-vlr_01 NEXT wa_saida_m-vlr_02.
          v_mes = sy-index.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = v_mes
            IMPORTING
              output = v_mes.
          CONCATENATE 'VLR_' v_mes INTO v_valor.
          ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida_abe TO <fs_val>.
          <fs_val> = <fs_val> * ( refe1 / 100 ).
        ENDDO.
      ENDIF.
      APPEND wa_saida_abe TO gt_saida.
    ENDLOOP.

  ENDLOOP.

  DATA: v_loopi TYPE i,
        v_loopf TYPE i,
        v_pos   TYPE i.

  v_loopi = p_poper-low.
  v_loopf = p_poper-high.
  IF v_loopf = 0.
    v_loopf = v_loopi.
  ENDIF.

  it_saida_aux[] = gt_saida[].
  DELETE it_saida_aux WHERE niv00 NE 'X'.
  REFRESH gt_saida_tot.
  LOOP AT it_saida_aux.
    CLEAR wa_saida_acu.
    MOVE-CORRESPONDING it_saida_aux TO wa_saida_acu.
    LOOP AT gt_saida INTO wa_saida WHERE nivel+0(2)  = it_saida_aux-nivel+0(2).
      IF wa_saida-niv00  IS INITIAL.
        CONTINUE.
      ENDIF.
      v_loopi = p_poper-low.

      WHILE v_loopi LE v_loopf.
        v_mes = v_loopi.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_mes
          IMPORTING
            output = v_mes.
        CONCATENATE 'VLR_' v_mes INTO v_valor.
        CONCATENATE 'VLR_' v_mes INTO v_total.
        ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida     TO <fs_val>.
        ASSIGN COMPONENT v_total OF STRUCTURE wa_saida_acu TO <fs_tot>.
        IF wa_saida-niv00 = '+'.
          <fs_tot> = <fs_tot> + <fs_val>.
        ELSE.
          <fs_tot> = <fs_tot> - <fs_val>.
        ENDIF.
        ADD 1 TO v_loopi.
      ENDWHILE.
    ENDLOOP.
    APPEND wa_saida_acu TO gt_saida_tot.
  ENDLOOP.

  LOOP AT gt_saida INTO wa_saida.
    IF wa_saida-niv00 NE 'X'.
      CONTINUE.
    ENDIF.
    tabix = sy-tabix.
    READ TABLE gt_saida_tot INTO wa_saida_tot WITH KEY niv00 = 'X'
                                                       nivel = wa_saida-nivel.
    IF sy-subrc = 0.
      MODIFY gt_saida FROM wa_saida_tot INDEX tabix.
    ENDIF.
  ENDLOOP.

  " 2 nivel
  it_saida_aux[] = gt_saida[].
  DELETE it_saida_aux WHERE niv00 NE 'T'.
  REFRESH gt_saida_tot.
  LOOP AT it_saida_aux.
    CLEAR wa_saida_acu.
    MOVE-CORRESPONDING it_saida_aux TO wa_saida_acu.
    LOOP AT gt_saida INTO wa_saida WHERE nivel+0(5)  = it_saida_aux-nivel+0(5).
      IF wa_saida-niv00  IS INITIAL.
        CONTINUE.
      ENDIF.
      v_loopi = p_poper-low.

      WHILE v_loopi LE v_loopf.
        v_mes = v_loopi.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_mes
          IMPORTING
            output = v_mes.
        CONCATENATE 'VLR_' v_mes INTO v_valor.
        CONCATENATE 'VLR_' v_mes INTO v_total.
        ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida     TO <fs_val>.
        ASSIGN COMPONENT v_total OF STRUCTURE wa_saida_acu TO <fs_tot>.
        IF wa_saida-niv00 = '+'.
          <fs_tot> = <fs_tot> + <fs_val>.
        ELSE.
          <fs_tot> = <fs_tot> - <fs_val>.
        ENDIF.
        ADD 1 TO v_loopi.
      ENDWHILE.
    ENDLOOP.
    APPEND wa_saida_acu TO gt_saida_tot.
  ENDLOOP.

  LOOP AT gt_saida INTO wa_saida.
    IF wa_saida-niv00 NE 'T'.
      CONTINUE.
    ENDIF.
    tabix = sy-tabix.
    READ TABLE gt_saida_tot INTO wa_saida_tot WITH KEY niv00 = 'T'
                                                       nivel = wa_saida-nivel.
    IF sy-subrc = 0.
      MODIFY gt_saida FROM wa_saida_tot INDEX tabix.
    ENDIF.
  ENDLOOP.
  " 3 nivel
  it_saida_aux[] = gt_saida[].
  DELETE it_saida_aux WHERE niv00 NE 'S'.
  REFRESH gt_saida_tot.
  LOOP AT it_saida_aux.
    CLEAR wa_saida_acu.
    MOVE-CORRESPONDING it_saida_aux TO wa_saida_acu.
    LOOP AT gt_saida INTO wa_saida WHERE nivel  = it_saida_aux-nivel.
      IF wa_saida-niv00  IS INITIAL.
        CONTINUE.
      ENDIF.
      v_loopi = p_poper-low.

      WHILE v_loopi LE v_loopf.
        v_mes = v_loopi.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_mes
          IMPORTING
            output = v_mes.
        CONCATENATE 'VLR_' v_mes INTO v_valor.
        CONCATENATE 'VLR_' v_mes INTO v_total.
        ASSIGN COMPONENT v_valor OF STRUCTURE wa_saida     TO <fs_val>.
        ASSIGN COMPONENT v_total OF STRUCTURE wa_saida_acu TO <fs_tot>.
        IF wa_saida-niv00 = '+'.
          <fs_tot> = <fs_tot> + <fs_val>.
        ELSE.
          <fs_tot> = <fs_tot> - <fs_val>.
        ENDIF.
        ADD 1 TO v_loopi.
      ENDWHILE.
    ENDLOOP.
    APPEND wa_saida_acu TO gt_saida_tot.
  ENDLOOP.

  LOOP AT gt_saida INTO wa_saida.
    IF wa_saida-niv00 NE 'S'.
      CONTINUE.
    ENDIF.
    tabix = sy-tabix.
    READ TABLE gt_saida_tot INTO wa_saida_tot WITH KEY niv00 = 'S'
                                                       nivel = wa_saida-nivel
                                                       saknr = ''.
    IF sy-subrc = 0.
      MODIFY gt_saida FROM wa_saida_tot INDEX tabix.
    ENDIF.
  ENDLOOP.



ENDFORM.
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  IF vg_rel = 'X'.
    EXIT.
  ENDIF.

  vg_rel = 'X'.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  SET PF-STATUS '0100' EXCLUDING fcode.
  SET TITLEBAR  '0100'.

  DATA: wa_mes LIKE t247,
        it_mes LIKE STANDARD TABLE OF wa_mes.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language    = sy-langu
    TABLES
      month_names = it_mes.


  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.

  IF tree1 IS NOT INITIAL.
    CALL METHOD tree1->free.
  ENDIF.

  IF l_custom_container IS NOT INITIAL.
    CALL METHOD l_custom_container->free.
  ENDIF.

  REFRESH: it_saida,gt_saida, lt_list_commentary.
  CLEAR: tree1, l_custom_container.

  IF tree1 IS INITIAL.
    PERFORM f_monta_layout.
    DATA: ls_fieldcatalog TYPE lvc_s_fcat.
    LOOP AT gt_fieldcatalog INTO ls_fieldcatalog.
      "VLR_01
      IF 'VLR' CS ls_fieldcatalog-fieldname+0(3).
        READ TABLE it_mes INTO wa_mes WITH KEY mnr = ls_fieldcatalog-fieldname+4(2).
        CONCATENATE '' wa_mes-ltx  INTO ls_fieldcatalog-coltext   SEPARATED BY space.
        CONCATENATE '' wa_mes-ltx  INTO ls_fieldcatalog-scrtext_l SEPARATED BY space.
        CONCATENATE '' wa_mes-ltx  INTO ls_fieldcatalog-scrtext_m SEPARATED BY space.
        CONCATENATE '' wa_mes-ltx  INTO ls_fieldcatalog-scrtext_s SEPARATED BY space.
        IF p_poper-high IS NOT   INITIAL.
          IF ls_fieldcatalog-fieldname+4(2) NOT BETWEEN p_poper-low+1(2) AND p_poper-high+1(2).
            ls_fieldcatalog-no_out = 'X'.
            ls_fieldcatalog-key    = ''.
          ENDIF.
        ELSEIF ls_fieldcatalog-fieldname+4(2) NE p_poper-low+1(2).
          ls_fieldcatalog-no_out = 'X'.
          ls_fieldcatalog-key    = ''.
        ENDIF.
      ELSE.
        ls_fieldcatalog-no_out = 'X'.
        ls_fieldcatalog-key    = ''.
      ENDIF.

      MODIFY gt_fieldcatalog FROM ls_fieldcatalog.
      CLEAR ls_fieldcatalog.
    ENDLOOP.

    l_tree_container_name = 'TREE1'.

    CREATE OBJECT l_custom_container
      EXPORTING
        container_name              = l_tree_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

    CREATE OBJECT tree1
      EXPORTING
        parent                      = l_custom_container
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
        item_selection              = space
        no_html_header              = ''
        no_toolbar                  = ''
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.
    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

    PERFORM f_hierarchy_header_ CHANGING l_hierarchy_header.

    PERFORM f_cabecalho USING lt_list_commentary.

    CALL METHOD tree1->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = l_hierarchy_header
        it_list_commentary  = lt_list_commentary
      CHANGING
        it_outtab           = it_saida[] "table must be emty !!
        it_fieldcatalog     = gt_fieldcatalog.

    DATA: vkey_nivel00 TYPE lvc_nkey,
          vkey_nivel01 TYPE lvc_nkey,
          vkey_nivel02 TYPE lvc_nkey,
          vkey_nivel03 TYPE lvc_nkey,
          vkey_nivel04 TYPE lvc_nkey,
          vkey_nivel05 TYPE lvc_nkey,
          vkey_saknr   TYPE lvc_nkey,
          vkey_kostl   TYPE lvc_nkey,
          vkey_prctr   TYPE lvc_nkey,
          vkey_matkl   TYPE lvc_nkey,
          vkey_ultimo  TYPE lvc_nkey.

    PERFORM f_saida.

    LOOP AT gt_saida INTO wa_saida.

      ON CHANGE OF wa_saida-nivel(2).
        PERFORM add_node_alv USING wa_saida '' CHANGING vkey_nivel01.
        vkey_nivel00 = vkey_nivel01.
      ENDON.

      ON CHANGE OF wa_saida-nivel+3(2).
        vg_nivel_est = wa_saida-nivel+3(2).
        IF NOT vg_nivel_est IS INITIAL.
          PERFORM add_node_alv USING  wa_saida vkey_nivel01 CHANGING vkey_nivel02.
          vkey_nivel00 = vkey_nivel02.
        ENDIF.
      ENDON.

      ON CHANGE OF wa_saida-nivel+6(2).
        vg_nivel_est = wa_saida-nivel+6(2).
        IF NOT vg_nivel_est IS INITIAL.
          PERFORM add_node_alv USING  wa_saida vkey_nivel02 CHANGING vkey_nivel03.
          vkey_nivel00 = vkey_nivel03.
        ENDIF.
      ENDON.

      ON CHANGE OF wa_saida-saknr.
        IF wa_saida-saknr IS NOT INITIAL.
          PERFORM add_node_saknr_alv USING wa_saida vkey_nivel00 CHANGING vkey_ultimo.
        ENDIF.
      ENDON.

      ON CHANGE OF wa_saida-matnr.
        IF wa_saida-matnr IS NOT INITIAL.
          IF wa_saida-nivel = '09'.
            PERFORM add_node_matnr_alv USING wa_saida vkey_nivel00 CHANGING  vkey_matkl.
          ELSE.
            PERFORM add_node_matnr_alv USING wa_saida vkey_nivel00 CHANGING  vkey_matkl.
          ENDIF.
        ENDIF.
      ENDON.
*
    ENDLOOP.

* calculate totals
    CALL METHOD tree1->update_calculations.

* this method must be called to send the data to the frontend
    CALL METHOD tree1->frontend_update.

*      optimize column-width
    CALL METHOD tree1->column_optimize
      EXPORTING
        i_start_column = tree1->c_hierarchy_column_name
        i_end_column   = tree1->c_hierarchy_column_name.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_layout .

  REFRESH gt_fieldcatalog.

  DATA: texto_01 TYPE scrtext_l,
        texto_02 TYPE scrtext_l,
        texto_03 TYPE scrtext_l,
        texto_04 TYPE scrtext_l,
        texto_05 TYPE scrtext_l,
        texto_06 TYPE scrtext_l,
        texto_07 TYPE scrtext_l,
        texto_08 TYPE scrtext_l,
        texto_09 TYPE scrtext_l,
        texto_10 TYPE scrtext_l,
        texto_11 TYPE scrtext_l,
        texto_12 TYPE scrtext_l,
        texto_13 TYPE scrtext_l,
        texto_14 TYPE scrtext_l,
        texto_15 TYPE scrtext_l,
        texto_16 TYPE scrtext_l.

  texto_01 = TEXT-001.
  texto_02 = TEXT-002.
  texto_03 = TEXT-003.
  texto_04 = TEXT-004.
  texto_05 = TEXT-005.
  texto_06 = TEXT-006.
  texto_07 = TEXT-007.
  texto_08 = TEXT-008.
  texto_09 = TEXT-009.
  texto_10 = TEXT-010.
  texto_11 = TEXT-011.
  texto_12 = TEXT-012.
  texto_13 = TEXT-013.
  texto_14 = TEXT-014.
  texto_15 = TEXT-015.
  texto_16 = TEXT-016.

  PERFORM montar_estrutura USING:
         1 ' '           ' '        'GT_SAIDA' 'NIVEL'      texto_01    '08' ' ' ' ' ' ',
         1 ' '           ' '        'GT_SAIDA' 'NITXT'      texto_02    '30' ' ' ' ' ' ',
         1 ' '           ' '        'GT_SAIDA' 'SAKNR'      texto_03    '10' ' ' ' ' ' ',
         1 ' '           ' '        'GT_SAIDA' 'TXT50'      texto_04    '30' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_01'     texto_05    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_02'     texto_06    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_03'     texto_07    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_04'     texto_08    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_05'     texto_09    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_06'     texto_10    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_07'     texto_11    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_08'     texto_12    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_09'     texto_13    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_10'     texto_14    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_11'     texto_15    '20' ' ' ' ' ' ',
         1 'BSIS'        'DMBTR'    'GT_SAIDA' 'VLR_12'     texto_16    '20' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
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
  w_fieldcatalog-coltext       = p_scrtext_l.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  APPEND w_fieldcatalog TO gt_fieldcatalog.

ENDFORM.                    " montar_estrutura

FORM f_hierarchy_header_  CHANGING p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = 'Itens'.
  p_hierarchy_header-tooltip = 'Itens'.
  p_hierarchy_header-width = 35.
  p_hierarchy_header-width_pix = ''.

ENDFORM.                    " F_HIERARCHY_HEADER_

FORM f_cabecalho  USING    p_list_commentary TYPE slis_t_listheader.

  DATA: wa_mes    LIKE t247,
        it_mes    LIKE STANDARD TABLE OF wa_mes,
        vg_titulo LIKE zgl001_dre_est-vstxt.

  DATA: vl_line    TYPE slis_listheader,
        vl_txt     TYPE c LENGTH 50,
        vl_txt2    TYPE c LENGTH 50,
        vl_empresa TYPE c LENGTH 50,
        vl_data    TYPE c LENGTH 14,
        vl_hora    TYPE c LENGTH 8,
        vl_name1   TYPE t001w-name1.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language    = sy-langu
    TABLES
      month_names = it_mes.

  READ TABLE it_mes INTO wa_mes WITH KEY mnr = p_poper-low.

  vl_txt = wa_mes-ltx.

  CONCATENATE vg_titulo '[' vl_txt  INTO vl_txt.

  IF p_poper-high IS INITIAL.
    CONCATENATE  vl_txt ']' INTO vl_txt.
  ELSE.
    READ TABLE it_mes INTO wa_mes WITH KEY mnr = p_poper-high.
    vl_txt2 = wa_mes-ltx.
    CONCATENATE  vl_txt  '-' vl_txt2 ']' INTO vl_txt.
  ENDIF.
  CONCATENATE  vl_txt 'de' p_bdatj INTO vl_txt SEPARATED BY space.
  CLEAR vl_line.
  vl_line-typ  = 'H'.
  vl_line-info = vl_txt.
  APPEND vl_line TO p_list_commentary.


  SELECT SINGLE name1
    INTO vl_name1
    FROM t001w
    WHERE werks = p_werks.

  CLEAR vl_line.
  vl_line-typ  = 'S'.
  vl_line-key  = 'Centro'.
  CONCATENATE p_werks '-' vl_name1 INTO vl_line-info SEPARATED BY space.
  APPEND vl_line TO p_list_commentary.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_matnr-low
    IMPORTING
      output = wa_saida-matnr.

  SELECT SINGLE *
    FROM  makt
    INTO wa_makt
    WHERE matnr = p_matnr-low.
  CLEAR vl_line.
  vl_line-typ  = 'S'.
  vl_line-key  = 'Material'.
  CONCATENATE wa_saida-matnr '-' wa_makt-maktx INTO vl_line-info SEPARATED BY space.
  APPEND vl_line TO p_list_commentary.

  CONCATENATE sy-datum+6(2)
              '.' sy-datum+4(2)
              '.' sy-datum(4) INTO vl_data.

  CONCATENATE sy-uzeit(2)
              ':' sy-uzeit+2(2)
              ':' sy-uzeit+4(2) INTO vl_hora.

  CONCATENATE  sy-uname 'em' vl_data '-' vl_hora INTO vl_txt SEPARATED BY space.

  CLEAR vl_line.
  vl_line-typ  = 'S'.
  vl_line-key  = 'Gerado por:'.
  vl_line-info = vl_txt.
  APPEND vl_line TO p_list_commentary.
ENDFORM.                    "F_CABECALHO


FORM add_node_alv  USING    pa_saida  TYPE ty_saida
                            vkey_relat      TYPE lvc_nkey
                    CHANGING p_vkey_nivel    TYPE lvc_nkey.

  DATA: l_node_text    TYPE lvc_value,
        lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA: ls_node TYPE lvc_s_layn.


  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  APPEND ls_item_layout TO lt_item_layout.
  CONCATENATE pa_saida-nivel '-' pa_saida-nitxt INTO l_node_text SEPARATED BY space.

  IF pa_saida-nivel = '01' OR pa_saida-nivel = '02'.
    CLEAR ls_node.
  ELSEIF  '01_02_07' CS pa_saida-nivel+0(2).
    ls_node-n_image   = icon_sum.
    ls_node-exp_image = icon_sum.
  ENDIF.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = vkey_relat
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = pa_saida
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_vkey_nivel.

ENDFORM.                    " ADD_NODE_ALV
FORM add_node_saknr_alv  USING    pa_saida TYPE ty_saida
                                  p_vkey_nivel00  TYPE lvc_nkey
                         CHANGING p_vkey_saknr    TYPE lvc_nkey.
  DATA: l_node_text TYPE lvc_value.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi,
        ls_node        TYPE lvc_s_layn,
        v_val(10),
        v_per(10),
        v_mes(02),
        v_loopi        TYPE i,
        v_loopf        TYPE i,
        v_pos          TYPE i.

  v_loopi = p_poper-low.
  v_loopf = p_poper-high.
  IF v_loopf = 0.
    v_loopf = v_loopi.
  ENDIF.

  WHILE v_loopi LE v_loopf.
    v_mes = v_loopi.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_mes
      IMPORTING
        output = v_mes.
    CONCATENATE 'VLR_' v_mes INTO v_val.

    ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
    APPEND ls_item_layout TO lt_item_layout.

    CLEAR ls_item_layout.
    ls_item_layout-fieldname = v_val.
    ls_item_layout-alignment = cl_gui_column_tree=>align_right.
    APPEND ls_item_layout TO lt_item_layout.

    ADD 1 TO v_loopi.
  ENDWHILE.

  CONCATENATE pa_saida-saknr '-' pa_saida-maktx INTO l_node_text SEPARATED BY space.
  ls_node-n_image   = icon_subscription.
  ls_node-exp_image = icon_outbox.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_vkey_nivel00
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = pa_saida
      i_node_text      = l_node_text
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_vkey_saknr.

ENDFORM.

FORM add_node_matnr_alv  USING  pa_saida TYPE ty_saida
                                 p_vkey_nivel00  TYPE lvc_nkey
                        CHANGING p_vkey_ultimo   TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi,
        ls_node        TYPE lvc_s_layn,
        v_val(10),
        v_per(10),
        v_mes(02),
        v_loopi        TYPE i,
        v_loopf        TYPE i,
        v_pos          TYPE i.

  v_loopi = p_poper-low.
  v_loopf = p_poper-high.
  IF v_loopf = 0.
    v_loopf = v_loopi.
  ENDIF.

  WHILE v_loopi LE v_loopf.
    v_mes = v_loopi.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_mes
      IMPORTING
        output = v_mes.
    CONCATENATE 'VLR_' v_mes INTO v_val.

    ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
    APPEND ls_item_layout TO lt_item_layout.

    CLEAR ls_item_layout.
    ls_item_layout-fieldname = v_val.
    ls_item_layout-alignment = cl_gui_column_tree=>align_right.
    APPEND ls_item_layout TO lt_item_layout.

    ADD 1 TO v_loopi.
  ENDWHILE.
  v_pos = strlen( pa_saida-nivel ).
  IF v_pos = 8.
    CONCATENATE pa_saida-nitxt '' INTO l_node_text SEPARATED BY space.
  ELSE.
    CONCATENATE pa_saida-matnr '-' pa_saida-maktx INTO l_node_text SEPARATED BY space.
    ls_node-n_image   = icon_biw_info_cube.
    ls_node-exp_image = icon_biw_info_cube.
  ENDIF.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_vkey_nivel00
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = pa_saida
      i_node_text      = l_node_text
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_vkey_ultimo.

ENDFORM.                    " ADD_NODE_COMPLETE

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
