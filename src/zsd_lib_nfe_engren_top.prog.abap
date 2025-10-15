report  zsd_lib_nfe_engren line-size 132.

*************************************************************************
**   TABELAS STANDARD                                                   *
*************************************************************************
tables:
  j_1bnfe_active,
  sscrfields.

type-pools: slis, abap, kkblo.
************************************************************************
*   TIPOS            (TY_...)                                          *
************************************************************************
types:
  begin of ty_active,
    docnum type j_1bdocnum,
    bukrs  type bukrs,
    branch type j_1bbranc_,
    model  type j_1bmodel,
  end   of ty_active.


*************************************************************************
**   ESTRUTURAS                                                         *
*************************************************************************
data:
  wa_layout     type slis_layout_alv,
  wa_access_key type j_1b_nfe_access_key,
  wa_alv        type zsds030,
  wa_active     type ty_active,
  wa_return     type zsds030,
  wa_status     type ztsd_stats_nfe,
  wa_stats_nfe  type ztmm_stats_nfe.

************************************************************************
*   TABELAS INTERNAS                                                   *
************************************************************************
data:
  t_fieldcat         type slis_t_fieldcat_alv,
  t_fieldcat_i       type slis_t_fieldcat_alv,
  t_events           type slis_t_event with header line,
  t_event_exit       type slis_t_event_exit with header line,
  t_list_top_of_page type slis_t_listheader,
  t_alv              type zsdc020,
  t_alv_lote         type zsdc020,
  t_bapiret2         type zbapiret2_50_tab,
  t_active           type standard table of ty_active,
  t_return           type zsdc020,
  t_status           type standard table of ztsd_stats_nfe.
************************************************************************
*   VARIÁVEIS GLOBAIS                                                  *
************************************************************************

data:
  v_sucesso         type i,
  v_erro            type i,
  v_texto           type acpi_zuonr,
  v_perce(15)       type c,
  v_tabname_header  type slis_tabname value 'T_ALV_H',
  v_tabname_item    type slis_tabname value 'T_ANLA_DET',
  v_repid           type sy-repid,
  v_variante        type disvariant,
  wa_variant        type disvariant,
  v_save            type c,
  v_user_command    type  slis_formname value 'F_DISPLAY_LOGS',
  v_pf_status_set   type  slis_formname value 'F_PF_STATUS_SET',
  v_pf_status_set_i type  slis_formname value 'F_PF_STATUS_SET_I',
  v_rfcdest         type rfcdest,
  v_oper(1)         type c,
  v_docnumref       type j_1bdocnum,
  ls_active         type j_1bnfe_active,
  v_msgty           type  symsgty,
  v_msgv1           type  symsgv,
  v_tabix           type sytabix.

************************************************************************
*   RANGES                                                             *
************************************************************************

ranges: s_bstat           for bkpf-bstat,
        r_rldnr           for faglflext-rldnr,
        r_bwasl	          for tabw-bwasl.

************************************************************************
*   CONSTANTES                                                         *
************************************************************************
constants: c_cope      type kokrs value 'COPE',             "#EC NEEDED
           c_afapl     type afapl value 'PACL',
           c_fi        type c LENGTH 2 value 'FI',
           c_param1    type c LENGTH 2 value 'AA',
           c_param2    type c LENGTH 50 value 'DERIV_AREA',
           c_ktopl     type ktopl value 'PCCL',
           c_msgty_s   type symsgty value 'S',    " Mensagem sucesso.
           c_msgty_e   type symsgty value 'E',    " Mensagem erro.
           c_msgid_zgl type symsgid value 'ZGL',  " Classe de mens. ZGL
           c_msgno_000 type symsgno value '000',  " Mensagem 000
           c_msgno_005 type symsgno value '005',  " Mensagem 005 - ini proc.
           c_msgno_006 type symsgno value '006',  " Mensagem 006 - fim proc.
           c_msgno_007 type symsgno value '007',  " Mensagem 007 - tot.reg.pr.
           c_msgno_008 type symsgno value '008',  " Mensagem 008 - tot.reg.gr.
           c_msgno_009 type symsgno value '009',  " Mensagem 009 - resp.proc.
           c_msgno_015 type symsgno value '015',  " Mensagem 015 - tot.reg.erro
           con_on      type c value 'X',
           con_off     type c value ' ',
           c_55        type j_1bmodel value '55'.

data: begin of g_icons,
        icon_green_light(50)  type c,
        icon_yellow_light(50) type c,
        icon_red_light(50)    type c,
      end of g_icons.

************************************************************************
*   FIELD-SYMBOLS                                                      *
************************************************************************
field-symbols: <fs_campo>   type any.
