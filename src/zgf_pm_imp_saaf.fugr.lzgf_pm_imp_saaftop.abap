FUNCTION-POOL zgf_pm_imp_saaf.              "MESSAGE-ID ..
* INCLUDE LZGF_PM_IMP_SAAFD...               " Local class definition
TYPES:BEGIN OF ty_zftpme_lubrificante.
        INCLUDE TYPE zftpme_lubrificante.
TYPES: plano    TYPE caufvd-warpl,
        namostra TYPE num10,
      END OF ty_zftpme_lubrificante,

      BEGIN OF ty_zftpme_filtros.
        INCLUDE TYPE zftpme_filtros.
TYPES: plano TYPE caufvd-warpl,
      END OF ty_zftpme_filtros,

      BEGIN OF ty_equi,
        equnr TYPE v_equi-equnr,
        tidnr TYPE v_equi-tidnr,
        eqktx TYPE v_equi-eqktx,
        swerk TYPE v_equi-swerk,
        daufn TYPE v_equi-daufn,
        aufnr TYPE v_equi-aufnr,
        eqtyp TYPE v_equi-eqtyp,
        eqart TYPE v_equi-eqart,
        warpl TYPE v_equi-warpl,
        iwerk TYPE v_equi-iwerk,
        hequi TYPE v_equi-hequi,
      END OF ty_equi,

      BEGIN OF ty_qpct,
        code     TYPE qpct-code,
        kurztext TYPE qpct-kurztext,
      END OF ty_qpct,

      BEGIN OF ty_ret,
        tipo    TYPE c  LENGTH 20,
        message TYPE bapiret2-message,
      END OF ty_ret,

      BEGIN OF ty_comb,
        comb   TYPE t370fld_mat-fluid_type,
        combt  TYPE t370fld_t-type_text,
        posto  TYPE t370fld_mat-station,
        postot TYPE t370fld_stn_t-type_text,
        quant  TYPE dec_16_02_s,
        ordem  TYPE aufnr,
        local  TYPE iflo-tplnr,
        localt TYPE iflo-pltxt,
      END OF ty_comb.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         maktx TYPE makt-maktx,
       END OF ty_mara.

TYPES: BEGIN OF ty_iflo,
         tplnr TYPE iflo-tplnr,
         pltxt TYPE iflo-pltxt,
       END OF ty_iflo.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA: BEGIN OF it_sistemas OCCURS 0,
        sistema TYPE zftpme_lubrificante-sistema,
      END OF it_sistemas.

DATA: BEGIN OF tl_param OCCURS 0,
        zval  TYPE ztparam-zval,
        const TYPE ztparam-const,
      END OF tl_param.

DATA: c_e(1)       TYPE c VALUE 'E',
      c_s(1)       TYPE c VALUE 'S',
      c_f(1)       TYPE c VALUE 'F',
      c_h(1)       TYPE c VALUE 'H',
      c_0(1)       TYPE c VALUE '0',
      c_1(1)       TYPE c VALUE '1',
      c_2(1)       TYPE c VALUE '2',
      c_3(1)       TYPE c VALUE '3',
      c_0010(4)    TYPE c VALUE '0010',
      c_0020(4)    TYPE c VALUE '0020',
      c_0030(4)    TYPE c VALUE '0030',
      c_0040(4)    TYPE c VALUE '0040',
      c_zppm001(7) TYPE c VALUE 'ZPPM001'.

*&---------------------------------------------------------------------*
*& Declaração de tabelas
*&---------------------------------------------------------------------*
DATA: "it_return    TYPE STANDARD TABLE OF bapiret2,
  it_equi      TYPE TABLE OF          ty_equi,
  it_ret       TYPE STANDARD TABLE OF ty_ret,
  it_retmed    TYPE STANDARD TABLE OF ty_ret,
  it_retmed1   TYPE STANDARD TABLE OF ty_ret,
  it_dimpt     TYPE STANDARD TABLE OF diimpt,
  it_dimpt_sup TYPE STANDARD TABLE OF diimpt,
  it_saida1    TYPE STANDARD TABLE OF ty_zftpme_lubrificante,
  it_saida2    TYPE STANDARD TABLE OF ty_zftpme_filtros,
  it_mara      TYPE                   ty_mara OCCURS 0,
  it_iflo      TYPE                   ty_iflo OCCURS 0,
  it_ranges    TYPE                   bapi_alm_order_listhead_ranges OCCURS 0,
  it_result    LIKE                   bapi_alm_order_listhead_result OCCURS 0,
  it_return    TYPE TABLE OF          bapiret2,
  it_erro      TYPE TABLE OF          bapiret2,
  it_sucess    TYPE TABLE OF          bapiret2,
  it_log       TYPE TABLE OF          zlogapont WITH HEADER LINE,
  it_not       TYPE TABLE OF          bapi2080_1,
  gt_msg       TYPE TABLE OF bapiret2 WITH HEADER LINE,
  imrg_usr     TYPE imrg_usr.

DATA: wa_input         TYPE ztpm_imp_do_saaf,
      wl_general_equi  TYPE bapi_itob,
      wl_specific_equi TYPE bapi_itob_eq_only,
      wl_return        TYPE bapiret2,
      et_return        TYPE TABLE OF bapiret2 WITH HEADER LINE,
      ls_return        TYPE bapiret2,
      wa_equi          TYPE ty_equi,
      wa_ret           TYPE ty_ret,
      wa_retmed        TYPE ty_ret,
      wa_retmed1       TYPE ty_ret,
      wa_return        TYPE bapiret2,
      wa_dimpt         TYPE diimpt,
      wa_point         TYPE impt,
      wa_value         TYPE imrg,
      wa_saida1        TYPE ty_zftpme_lubrificante,
      wa_saida2        TYPE ty_zftpme_filtros,
      wa_status        TYPE bapi2080_notusrstati,
      wa_comb          TYPE ty_comb,
      wa_ranges        TYPE bapi_alm_order_listhead_ranges,
      wa_result        TYPE bapi_alm_order_listhead_result,
      wa_log           TYPE zlogapont,
      wa_not           TYPE bapi2080_1.

DATA: lv_equip             TYPE equi-equnr,
      lv_valor             TYPE ztparam-zval,
      lv_reader            TYPE sy-uname,
      lv_operacao(6)       TYPE n,
      lv_valr1             TYPE ztparam-zval,
      lv_valr2             TYPE ztparam-zval,
      lv_tabix             TYPE sy-tabix,
      lv_achou             TYPE c LENGTH 1,
      lv_matnr             TYPE mara-matnr,
      w_zpmt006            TYPE zpmt006,
      lv_ja_apontou(1)     TYPE c,
      vcont                TYPE i,
      vcont_sup            TYPE i,
      lv_hr_i              TYPE p,
      lv_hr_c              TYPE c LENGTH 6,
      vl_cont              TYPE rihimrg-pyeac,
      p_erro,
      lv_dt_inicio_mov     TYPE ztpm_imp_do_saaf-dt_inicio_mov,
      lv_s_hora_inicio_mov TYPE ztpm_imp_do_saaf-s_hora_inicio_mov,
*      IT_EXPORTACAO_COMMIT_SAAF TYPE TABLE OF ZTPM_EXP_COMMIT_SAAF,
      lv_erro.

DATA: t_doc_med TYPE TABLE OF imrg_mdocm.

DATA: tl_equi   TYPE TABLE OF equi     WITH HEADER LINE.
*&---------------------------------------------------------------------*
*& Declaração de Variáveis
*&---------------------------------------------------------------------*
DATA: v_equip      TYPE equi-equnr,
      v_equnr_sup  TYPE equi-equnr,
      v_super_desc TYPE v_equi-eqktx,
      v_locas      TYPE imptt-locas.

DATA: ok_code           TYPE sy-ucomm,
      v_medano          TYPE dec_16_02_s,
      v_medanh          TYPE dec_16_02_s,
      v_medato          TYPE dec_16_02_s,
      v_medath          TYPE dec_16_02_s,
      v_erro            TYPE c LENGTH 1,
      v_nerro           TYPE c LENGTH 1,
      v_nbaixa          TYPE c LENGTH 1,
      v_tipo            TYPE c LENGTH 50,
      v_tabix           TYPE sy-tabix,
      v_equnr           TYPE equi-equnr,
      v_pointo          TYPE imrc_point,
      v_pointh          TYPE imrc_point,
      v_itemo           LIKE wa_dimpt-psort,
      v_itemh           LIKE wa_dimpt-psort,
      v_unido           LIKE wa_dimpt-mrngu,
      v_unidh           LIKE wa_dimpt-mrngu,
      v_erroo           TYPE xfeld,
      v_erroh           TYPE xfeld,
      vtpponto          TYPE c LENGTH 10,
      vtpponto_sup      TYPE c LENGTH 10,
      vtpfiltro         TYPE c LENGTH 1,
      wg_erro           TYPE c LENGTH 1,
      v_dt_ponto        TYPE sy-datum,
      v_hr_ponto        TYPE sy-uzeit,
      v_hr_pto          TYPE c LENGTH 5,
      v_ans             TYPE c LENGTH 1,
      v_pt_comb         LIKE wa_dimpt-point,
      gv_dt_nota_aberto TYPE n LENGTH 5,
      string_form       TYPE imrg-mdtxt,            "/Modificação CS2016001593
      v_n_form          TYPE numc10,                "/Modificação CS2016001593
      _text_sort        TYPE char40.


DATA: rg_matnr     TYPE RANGE OF matnr,
      vg_matnr_aux TYPE char18.



*----------------------------------------------------------------------*
* Declarações SHDB
*----------------------------------------------------------------------*
DATA: it_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      t_messtab  TYPE TABLE OF          bdcmsgcoll,
      wa_bdcdata LIKE LINE OF           it_bdcdata.

CLASS zcl_imp DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_msg IMPORTING input         TYPE bapi_fld
                        tabix         TYPE sy-tabix
              RETURNING VALUE(return) TYPE char10.
ENDCLASS.

DATA obj_imp TYPE REF TO zcl_imp.

CLASS zcl_imp IMPLEMENTATION.
  METHOD get_msg.
    SPLIT input AT '#' INTO TABLE DATA(itab).
    return = itab[ tabix ].
  ENDMETHOD.
ENDCLASS.
