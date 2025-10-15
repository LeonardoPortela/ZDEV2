*&---------------------------------------------------------------------*
*&  ZFIR0043
*&
*&---------------------------------------------------------------------*

program  zfir0043.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
type-pools: slis.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

types: begin of ty_zib_contabil.
         include structure zib_contabil.
types:   mark type c,
       end of ty_zib_contabil.

types: begin of ty_zfit0060.
         include structure zfit0060.
types:   dif type i,
       end of ty_zfit0060.

types:
  begin of ty_cadpro,
    cod_oper      type zfit0065-cod_oper,
    descr_oper    type zfit0063-descr,
    bukrs         type zfit0062-bukrs,
    butxt         type t001-butxt,
    ptax          type zfit0060-tx_par_dolar,
    mensagem(100),
    dt_fechamento type zfit0060-dt_fechamento,
    belnr         type zib_contabil_chv-belnr,
    belnr2        type zib_contabil_chv-belnr,
    path(250),
    p_divida(1),
  end of ty_cadpro,

  begin of ty_taxas,
    seq             type zfit0060-seq,
    dias_corridos   type zfit0060-dias_corridos,
    tx_proj_ano_ban type zfit0060-tx_proj_ano_ban,
    tx_proj_ano_com type zfit0060-tx_proj_ano_com,
    tx_par_dolar    type zfit0060-tx_par_dolar,
    tx_cupom_camb   type zfit0060-tx_cupom_camb,
  end of ty_taxas,

  begin of ty_itens4,
    mark(1),
    cod_oper  type zfit0063-cod_oper,
    descr     type zfit0063-descr,
    tp_ajuste type zfit0066-tp_ajuste,
    bschl     type zfit0066-bschl,
    hkont     type zfit0066-hkont,
    txt50     type skat-txt50,
    shkzg     type tbsl-shkzg,
    wrbtr     type zib_contabil-wrbtr,
  end of ty_itens4,

  begin of ty_fields,
    campo(30) type c,
    group1(5) type c,
    value     type sy-tabix,
    invisible type sy-tabix,
  end   of ty_fields,

  begin of ty_editor,
    line(72),
  end   of ty_editor,

  begin of ty_zib_contabil_err,
    obj_key        type zib_contabil_err-obj_key,
    nr_item        type zib_contabil_err-nr_item,
    interface      type zib_contabil_err-interface,
    dt_atualizacao type zib_contabil_err-dt_atualizacao,
    hr_atualizacao type zib_contabil_err-hr_atualizacao,
    type           type zib_contabil_err-type,
    id             type zib_contabil_err-id,
    num            type zib_contabil_err-num,
    message        type zib_contabil_err-message,
    message_v1     type zib_contabil_err-message_v1,
    message_v2     type zib_contabil_err-message_v2,
    message_v3     type zib_contabil_err-message_v3,
    message_v4     type zib_contabil_err-message_v4,
  end of ty_zib_contabil_err.



*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
data: ok-code             type sy-ucomm,
      wg_cadpro           type ty_cadpro,
      wa_taxas            type ty_taxas,

      wa_zfit0059         type zfit0059,
      wa_zfit0083         type zfit0083,
      wa_zfit0095         type zfit0095,
      wa_zfit0060         type ty_zfit0060,
      wa_zfit0060_2       type ty_zfit0060,
      wa_zfit0061         type zfit0061,
      wa_zfit0062         type zfit0062,
      wa_zfit0062a        type zfit0062,
      wa_zfit0064         type zfit0064,
      wa_zfit0065         type zfit0065,
      wa_zfit0066         type zfit0066,
      wa_zfit0067         type zfit0067,
      wa_zfit0069         type zfit0069,
      wa_zfit0070         type zfit0070,
      wa_zfit0071         type zfit0071,
      wa_zib_contabil     type ty_zib_contabil,
      wa_zib_contabil_est type ty_zib_contabil,
      wa_zib_contabil_chv type zib_contabil_chv,
      wa_zib_contabil_err type zib_contabil_err,

      it_zfit0059         type table of zfit0059,
      it_zfit0083         type table of zfit0083,
      it_zfit0060         type table of ty_zfit0060,
      it_zfit0061         type table of zfit0061,
      it_zfit0062         type table of zfit0062,
      it_zfit0064         type table of zfit0064,
      it_zfit0065         type table of zfit0065,
      it_zfit0066         type table of zfit0066,
      it_zfit0067         type table of zfit0067,
      it_zfit0069         type table of zfit0069,
      it_zfit0070         type table of zfit0070,
      it_zfit0071         type table of zfit0071,
      t_usermd            type standard table of  rgsb4 with header line,
      it_zib_contabil     type table of ty_zib_contabil,
      it_zib_contabil_est type table of ty_zib_contabil,

      tg_selectedcell     type lvc_t_cell,
      wg_selectedcell     type lvc_s_cell,
      x_field(30),
      wg_mensagem(30),
      wg_acao(30),
      xmodif(1),
      xbloqueio(1),
      xadd(1),
      xbotao(1),
      xcalculo(1),
      xcalculo_ctb(1),
      xgravado(1),
      xestorno(1),
      xestorno_botao(1),
      xcod_oper_nav(1),
      vobj_key            type  zib_contabil-obj_key,
      robj_key            type  zib_contabil-obj_key,

      btn_ant(30)         value '@0D@ Anterior',
      btn_prox(30)        value '@0E@ Próximo',
      btn_a(30)           value '@0D@ ',
      btn_p(30)           value '@0E@ '.

field-symbols: <wa_zfit0083> type zfit0083,
               <wa_0083_del> type zfit0083.

data: t_empresa type standard table of  rgsb4 with header line,
      check_emp type c.

" Calcula Ultimo dia do mês.
data: data_ini      type sy-datum,
      vmes(2),
      vano(4),
      nmes          type i,
      nano          type i,
      vdata01       type sy-datum,
      p_data_val    type datum,
      vg_extrato(1),
      wl_color      type kkblo_specialcol,

**DECLARAÇÃO DE VARIÁVEIS DE TAXA DE CÂMBIO
      xtx_eur       type tcurr-ukurs,
      xtx_usd       type tcurr-ukurs,

      begin of tg_itens occurs 0,
        mark(1),
        seq             type zfit0060-seq,
        dias_corridos   type zfit0060-dias_corridos,
        tx_proj_ano_ban type zfit0060-tx_proj_ano_ban,
        tx_proj_ano_com type zfit0060-tx_proj_ano_com,
        tx_par_dolar    type zfit0060-tx_par_dolar,
        dt_base_bmf     type zfit0060-dt_base_bmf,
        dias_uteis      type zfit0060-dias_uteis,
        tx_cupom_camb   type zfit0060-tx_par_dolar,
      end of tg_itens,

      begin of tg_itens2 occurs 0,
        mark(1),
        dt_vcto_cto      type zfit0062-dt_vcto_cto,
        seq              type zfit0062-seq,
        dias_c_mtm_cto   type zfit0062-dias_c_mtm_cto,
        dias_c_pri_inter type zfit0062-dias_c_pri_inter,
        dias_c_seq_inter type zfit0062-dias_c_seq_inter,
        dias_u_mtm_cto   type zfit0062-dias_u_mtm_cto,
        dias_u_pri_inter type zfit0062-dias_u_pri_inter,
        dias_u_seq_inter type zfit0062-dias_u_seq_inter,
        tx_bco_tp        type zfit0062-tx_bco_tp,
        tx_bco_tm        type zfit0062-tx_bco_tm,
        tx_bco_interp    type zfit0062-tx_bco_interp,
        tx_com_t1p       type zfit0062-tx_com_t1p,
        tx_com_t1m       type zfit0062-tx_com_t1m,
        tx_com_interp    type zfit0062-tx_com_interp,
        tx_cb_tp         type zfit0062-tx_cb_tp,
        tx_cb_tm         type zfit0062-tx_cb_tm,
        tx_cb_interp     type zfit0062-tx_cb_interp,
      end of tg_itens2,

      begin of tg_itens3 occurs 0,
        mark(1),
        bukrs            type zfit0064-bukrs,
        nro_cto          type zfit0064-nro_cto,
        cod_oper         type zfit0064-cod_oper,
        banco            type zfit0064-banco,
        dt_inicio_cto    type zfit0064-dt_inicio_cto,
        dt_fim_cto       type zfit0064-dt_fim_cto,
        tp_posicao       type zfit0064-tp_posicao,
        natureza_cto     type zfit0064-natureza_cto,
        tp_operacao      type zfit0064-tp_operacao,
        vlr_operacao     type zfit0064-vlr_operacao,
        moeda            type zfit0064-moeda,
        tx_cambio_fut    type zfit0064-tx_cambio_fut,
        qte_dias_dt_base type zfit0064-qte_dias_dt_base,
        vlr_base_calc    type zfit0064-vlr_base_calc,
        tx_camb_proj     type zfit0064-tx_camb_proj,
        vlr_fut_proj     type zfit0064-vlr_fut_proj,
        tx_desc          type zfit0064-tx_desc,
        fator_desc       type zfit0064-fator_desc,
        vlr_extrato_bco  type zfit0064-vlr_extrato_bco,
        vlr_pres_mtm     type zfit0064-vlr_pres_mtm,
        tp_ajuste        type zfit0064-tp_ajuste,
        hkont_d          type zfit0066-hkont,
        hkont_c          type zfit0066-hkont,
        ano(4),
        color            type   kkblo_specialcol occurs 0,
      end of tg_itens3,

      begin of tg_itens4 occurs 0,
        mark(1),
        cod_oper    type zfit0063-cod_oper,
        descr       type zfit0063-descr,
        tp_operacao type zfit0064-tp_operacao,
        tp_ajuste   type zfit0066-tp_ajuste,
        bschl       type zfit0066-bschl,
        hkont       type zfit0066-hkont,
        txt50       type skat-txt50,
        shkzg       type tbsl-shkzg,
        wrbtr       type zib_contabil-wrbtr,
        grav(1),
      end of tg_itens4,

      begin of tg_itens4_aux occurs 0,
        mark(1),
        cod_oper    type zfit0063-cod_oper,
        descr       type zfit0063-descr,
        tp_operacao type zfit0064-tp_operacao,
        tp_ajuste   type zfit0066-tp_ajuste,
        bschl       type zfit0066-bschl,
        hkont       type zfit0066-hkont,
        txt50       type skat-txt50,
        shkzg       type tbsl-shkzg,
        wrbtr       type zib_contabil-wrbtr,
        grav(1),
      end of tg_itens4_aux,

      begin of tg_itens5 occurs 0,
        mark(1),
        bukrs            type zfit0067-bukrs,
        nro_cto          type zfit0067-nro_cto,
        nro_par          type zfit0067-nro_par,
        cod_oper         type zfit0067-cod_oper,
        banco            type zfit0067-banco,
        dt_inicio_cto    type zfit0067-dt_inicio_cto,
        dt_fim_cto       type zfit0067-dt_fim_cto,
        tp_tx_ativa      type zfit0067-tp_tx_ativa,
        tx_index_ativo   type zfit0067-tx_index_ativo,
        tx_cambio_in_op  type zfit0067-tx_cambio_in_op,
        tx_jros_ativa    type zfit0067-tx_jros_ativa,
        tp_posicao       type zfit0067-tp_posicao,
        natureza_cto     type zfit0067-natureza_cto,
        tp_operacao      type zfit0067-tp_operacao,
        vlr_operacao     type zfit0067-vlr_operacao,
        vlr_operacao_int type zfit0067-vlr_operacao_int,
        vlr_parc         type zfit0067-vlr_parc,
        vlr_parc_brl     type zfit0067-vlr_parc,
        vlr_amortiz      type zfit0067-vlr_juros_fut,
        moeda            type zfit0067-moeda,
        tp_tx_passiva    type zfit0067-tp_tx_passiva,
        tx_index_passivo type zfit0067-tx_index_passivo,
        tx_jros_passiva  type zfit0067-tx_jros_passiva,
        dias_corridos_01 type zfit0067-dias_corridos_01,
        dias_uteis_01    type zfit0067-dias_uteis_01,
        dias_corridos_02 type zfit0067-dias_corridos_02,
        dias_uteis_02    type zfit0067-dias_uteis_02,
        dias_corridos_03 type zfit0067-dias_corridos_03,
        dias_uteis_03    type zfit0067-dias_uteis_03,
        vlr_aux_op_tp1   type zfit0067-vlr_aux_op_tp1,
        vlr_aux_op_tp2   type zfit0067-vlr_aux_op_tp2,
        vlr_aux_op_tp3   type zfit0067-vlr_aux_op_tp3,
        vlr_proj_jros    type zfit0067-vlr_proj_jros,
        tx_desc_cdi      type zfit0067-tx_desc_cdi,
        vlr_mtm_p_ativ   type zfit0067-vlr_mtm_p_ativ,
        tx_proj_interp   type zfit0067-tx_proj_interp,
        vlr_var_proj     type zfit0067-vlr_var_proj,
        vlr_var_cambial  type zfit0067-vlr_var_cambial,
        vlr_juros_fut    type zfit0067-vlr_juros_fut,
        tx_desc          type zfit0067-tx_desc,
        vlr_mtm_p_passiv type zfit0067-vlr_mtm_p_passiv,
        vlr_aj_merc      type zfit0067-vlr_aj_merc,
        vlr_extrato_bco  type zfit0067-vlr_extrato_bco,
        a_ind_var_db     type zfit0067-a_ind_var_db,
        a_ind_fix_db     type zfit0067-a_ind_fix_db,
        a_pa_curva       type zfit0067-a_pa_curva,
        a_cdi_interp     type zfit0067-a_cdi_interp,
        a_ind_final      type zfit0067-a_ind_final,
        a_vf_pa          type zfit0067-a_vf_pa,
        a_vf_pa_ar       type zfit0067-a_vf_pa_ar,
        a_desc_cdi       type zfit0067-a_desc_cdi,
        p_moeda_comp     type zfit0067-p_moeda_comp,
        p_fator_adic     type zfit0067-p_fator_adic,
        p_fut_pp         type zfit0067-p_fut_pp,
        p_fut_pp_ar      type zfit0067-p_fut_pp_ar,
        p_desc_cdi       type zfit0067-p_desc_cdi,
        tp_ajuste        type zfit0067-tp_ajuste,
        csd_div_usd      type zfit0067-vlr_operacao,
        ano(4),
        hkont_d          type zfit0066-hkont,
        hkont_c          type zfit0066-hkont,
        color            type   kkblo_specialcol occurs 0,
      end of tg_itens5,

      begin of tg_itens6 occurs 0,
        mark(1),
        bukrs            type zfit0069-bukrs,
        nro_cto          type zfit0069-nro_cto,
        tx_data_base     type zfit0069-tx_data_base,
        cod_oper         type zfit0069-cod_oper,
        banco            type zfit0069-banco,
        dt_inicio_cto    type zfit0069-dt_inicio_cto,
        dt_fim_cto       type zfit0069-dt_fim_cto,
        vlr_operacao     type zfit0069-vlr_operacao,
        tx_cambio_in_op  type zfit0069-tx_cambio_in_op,
        vlr_operacao_int type zfit0069-vlr_operacao_int,
        index_ativo      type zfit0069-index_ativo,
        tx_index_ativo   type zfit0069-tx_index_ativo,
        tx_al_index_ativ type zfit0069-tx_al_index_ativ,
        tp_taxa_ativo    type zfit0069-tp_taxa_ativo,
        index_passivo    type zfit0069-index_passivo,
        tx_index_passivo type zfit0069-tx_index_passivo,
        tx_al_index_pass type zfit0069-tx_al_index_pass,
        tp_taxa_passivo  type zfit0069-tp_taxa_passivo,
        dias_corridos_01 type zfit0069-dias_corridos_01,
        dias_uteis_01    type zfit0069-dias_uteis_01,
        dias_corridos_02 type zfit0069-dias_corridos_02,
        dias_uteis_02    type zfit0069-dias_uteis_02,
        dias_corridos_03 type zfit0069-dias_corridos_03,
        dias_uteis_03    type zfit0069-dias_uteis_03,
        tx_comp_dolar_at type zfit0069-tx_comp_dolar_at,
        ft_atual_ativa   type zfit0069-ft_atual_ativa,
        vlr_curva_ativa  type zfit0069-vlr_curva_ativa,
        ft_acum_indev    type zfit0069-ft_acum_indev,
        ft_multp_passiva type zfit0069-ft_multp_passiva,
        vlr_curva_passiv type zfit0069-vlr_curva_passiv,
        tx_proj_interp   type zfit0069-tx_proj_interp,
        tx_inter_dolar_a type zfit0069-tx_inter_dolar_a,
        tx_inter_dolar_p type zfit0069-tx_inter_dolar_p,
        ft_multp_ativa   type zfit0069-ft_multp_ativa,
        vlr_fut_ativa    type zfit0069-vlr_fut_ativa,
        tx_inter_cdi_ati type zfit0069-tx_inter_cdi_ati,
        vlr_mtm_p_ativ   type zfit0069-vlr_mtm_p_ativ,
        proj_cdi_inter   type zfit0069-proj_cdi_inter,
        ind_proj_final   type zfit0069-ind_proj_final,
        ind_proj_final2  type zfit0069-ind_proj_final,
        vlr_fut_passiva  type zfit0069-vlr_fut_passiva,
        tx_inter_cdi_pas type zfit0069-tx_inter_cdi_pas,
        vlr_mtm_p_passiv type zfit0069-vlr_mtm_p_passiv,
        vlr_extrato_bco  type zfit0069-vlr_extrato_bco,
        vlr_aj_merc      type zfit0069-vlr_aj_merc,
        tp_ajuste        type zfit0069-tp_ajuste,
        p_cdi_interp     type zfit0069-p_cdi_interp,
        p_ind_final      type zfit0069-p_ind_final,
        moeda_comparada  type zfit0069-moeda_comparada,
        saldo_divida     type zfit0069-saldo_divida,
        hkont_d          type zfit0066-hkont,
        hkont_c          type zfit0066-hkont,
        ano(4),
        tipo_opera(20),
        color            type   kkblo_specialcol occurs 0,
      end of tg_itens6,

      begin of tg_itens7 occurs 0,
        mark(1),
        bukrs            type zfit0070-bukrs,
        nro_cto          type zfit0070-nro_cto,
        nro_par          type zfit0070-nro_par,
        tx_data_base     type zfit0070-tx_data_base,
        cod_oper         type zfit0070-cod_oper,
        banco            type zfit0070-banco,
        dt_inicio_cto    type zfit0070-dt_inicio_cto,
        dt_fim_cto       type zfit0070-dt_fim_cto,
        tp_tx_ativa      type zfit0070-tp_tx_ativa,
        index_ativo      type zfit0070-index_ativo,
        tx_cambio_in_op  type zfit0070-tx_cambio_in_op,
        tx_perct_cdi     type zfit0070-tx_perct_cdi,
        tx_jrs_alem_cdi  type zfit0070-tx_jrs_alem_cdi,
        vlr_operacao     type zfit0070-vlr_operacao,
        vlr_operacao_int type zfit0070-vlr_operacao_int,
        moeda            type zfit0070-moeda,
        tp_tx_passiva    type zfit0070-tp_tx_passiva,
        vlr_base_vc      type zfit0070-vlr_base_vc,
        tx_jros_passiva  type zfit0070-tx_jros_passiva,
        dias_corridos_01 type zfit0070-dias_corridos_01,
        dias_uteis_01    type zfit0070-dias_uteis_01,
        dias_corridos_02 type zfit0070-dias_corridos_02,
        dias_uteis_02    type zfit0070-dias_uteis_02,
        dias_corridos_03 type zfit0070-dias_corridos_03,
        dias_uteis_03    type zfit0070-dias_uteis_03,
        ft_acum_indev    type zfit0070-ft_acum_indev,
        ft_multp_ativa   type zfit0070-ft_multp_ativa,
        vlr_curva_ativa  type zfit0070-vlr_curva_ativa,
        tx_comp_dolar_pa type zfit0070-tx_comp_dolar_pa,
        ft_atual_passiva type zfit0070-ft_atual_passiva,
        vlr_curva_passiv type zfit0070-vlr_curva_passiv,
        proj_cdi_inter   type zfit0070-proj_cdi_inter,
        ind_proj_cdi_int type zfit0070-ind_proj_cdi_int,
        vlr_proj_jros    type zfit0070-vlr_proj_jros,
        tx_desc_cdi      type zfit0070-tx_desc_cdi,
        vlr_mtm_p_ativ   type zfit0070-vlr_mtm_p_ativ,
        tx_proj_interp   type zfit0070-tx_proj_interp,
        vlr_var_proj     type zfit0070-vlr_var_proj,
        vlr_var_cambial  type zfit0070-vlr_var_cambial,
        vlr_juros_fut    type zfit0070-vlr_juros_fut,
        tx_desc          type zfit0070-tx_desc,
        vlr_mtm_p_passiv type zfit0070-vlr_mtm_p_passiv,
        vlr_extrato_bco  type zfit0070-vlr_extrato_bco,
        vlr_aj_merc      type zfit0070-vlr_aj_merc,
        tp_ajuste        type zfit0070-tp_ajuste,
        hkont_d          type zfit0066-hkont,
        hkont_c          type zfit0066-hkont,
        p_moeda_comp     type zfit0070-p_moeda_comp,
        tx_index_passivo type zfit0070-tx_index_passivo,
        ano(4),
        color            type   kkblo_specialcol occurs 0,
      end of tg_itens7.


data tg_itens6_aux like table of tg_itens6.
** Criação de tabela dinamica
data: t_fieldcatalog      type lvc_t_fcat,
      w_fieldcatalog      type lvc_s_fcat,
      t_sort              type lvc_t_sort with header line,
      wa_layout           type lvc_s_layo,
      wa_stable           type lvc_s_stbl,
      wg_editor           type ty_editor,
      wg_colaps(4)        value '@K2@',
      btn_rei(15)         type c,

      v_dias_uteis        type i,
      v_ft_acum_indev     type p decimals 8, " ZFIT0069-FT_ACUM_INDEV,
      tabix               type sy-tabix,

      it_zib_contabil_err type table of ty_zib_contabil_err  with header line,
      tg_fields           type table of ty_fields   with header line,
      tg_editor           type table of ty_editor,
      tg_editor1          type table of ty_editor,
      tg_editor2          type table of ty_editor,
      tg_msg_ret          type table of zfiwrs0002 with header line.

*&----------------------------------------------------------------------*
*&  Variaveis do texto
*&----------------------------------------------------------------------*
data: wl_cont      type sy-tabix,
      wl_cont_aux  type sy-tabix,
      wl_cont_aux2 type sy-tabix,
      wl_zfit0063  type zfit0063,
      wl_t001      type t001.

*&----------------------------------------------------------------------*
*&  Objetos Load Imagem
*&----------------------------------------------------------------------*
data: w_lines         type i.
types pict_line(256)  type c.
data : container     type ref to cl_gui_custom_container,
       container1    type ref to cl_gui_custom_container,
       container2    type ref to cl_gui_custom_container,
       container3    type ref to cl_gui_custom_container,
       editor        type ref to cl_gui_textedit,
       picture       type ref to cl_gui_picture,
       picture1      type ref to cl_gui_picture,
       picture2      type ref to cl_gui_picture,
       picture3      type ref to cl_gui_picture,
       pict_tab      type table of pict_line,
       obj_dyndoc_id type ref to cl_dd_document,
       editcontainer type ref to cl_gui_custom_container,

       url(255)      type c.

data: graphic_url(255).

data: begin of graphic_table occurs 0,
        line(255) type x,
      end of graphic_table.

data: l_graphic_conv type i.
data: l_graphic_offs type i.
data: graphic_size   type i.
data: l_graphic_xstr type xstring.

data lo_container      type ref to cl_gui_custom_container.
data lo_pic            type ref to cl_gui_picture.

*Class definition for ALV toolbar
"CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
data: g_container          type scrfname value 'CC_BMF',
      g_custom_container   type ref to cl_gui_custom_container,
      container_1          type ref to cl_gui_container,       "splitter conteiner 1
      container_2          type ref to cl_gui_container,       "splitter conteiner 2
      splitter             type ref to cl_gui_splitter_container,
      grid1                type ref to cl_gui_alv_grid,
      "OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      g_descbox            type scrfname value 'CC_DESC',
      g_custom_cont_desc   type ref to cl_gui_custom_container,
      obg_descbox          type ref to cl_gui_textedit,
      obg_docking          type ref to cl_gui_docking_container,

      wa_style             type lvc_s_styl,
      style                type lvc_t_styl  with header line,
      style2               type lvc_t_styl with header line,

      gs_variant_c         type disvariant.

data: begin of g_tab_tela,
        subscreen like sy-dynnr,
        prog      like sy-repid value 'ZFIR0043',
        subtela   like sy-dynnr,
      end of g_tab_tela,

      vsubtela type i.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
constants: c_0               type c value '0',
           c_1               type c value '1',
           c_2               type c value '2',
           c_b               type c value 'B',
           c_s               type c value 'S',
           c_l               type c value 'L',
           c_x               type c value 'X',
           c_d               type c value 'D',
           c_k               type c value 'K',
           c_w               type c value 'W',
           c_f               type c value 'F',
           c_t               type c value 'T',
           c_i               type c value 'I',
           c_n               type c value 'N',
           c_h               type c value 'H',
           c_ag(2)           type c value 'AG',
           c_ne(2)           type c value 'NE',
           c_01(2)           type c value '01',
           c_30(2)           type c value '30',
           c_40(2)           type c value '40',
           c_50(4)           type c value '0050',
           c_76(2)           type c value '76',
           c_71(2)           type c value '71',
           c_72(2)           type c value '72',
           c_br(2)           type c value 'BR',
           c_lf(2)           type c value 'LF',
           c_lr(2)           type c value 'LR',
           c_z1(2)           type c value 'Z1',
           c_add(3)          type c value 'ADD',
           c_del(3)          type c value 'DEL',
           c_dg1(3)          type c value 'DG1',
           c_dg2(3)          type c value 'DG2',
           c_dummy_header(3) type c value '099',
           c_dummy_itens(3)  type c value '098',
           c_exit(4)         type c value 'EXIT',
           c_root(4)         type c value 'ROOT',
           c_minimizar(4)    type c value '@K2@',
           c_maximizar(4)    type c value '@K1@',
           c_back(4)         type c value 'BACK',
           c_save(4)         type c value 'SAVE',
           c_desat(5)        type c value 'DESAT',
           c_dmbtr(5)        type c value 'DMBTR',
           c_modif(5)        type c value 'MODIF',
           c_cancel(6)       type c value 'CANCEL',
           c_deldoc(6)       type c value 'DELDOC',
           c_dclick(6)       type c value 'DCLICK',
           c_search(6)       type c value 'SEARCH',
           c_atuali(6)       type c value 'ATUALI',
           c_add_msg(7)      type c value 'ADD_MSG',
           c_del_msg(7)      type c value 'DEL_MSG',
           c_clos_msg(8)     type c value 'CLOS_MSG',
           c_save_msg(8)     type c value 'SAVE_MSG',
           c_col_exp(7)      type c value 'COL_EXP',
           c_displa(6)       type c value 'DISPLA',
           c_show_msgre(10)  type c value 'SHOW_MSGRE'.

*CONTROLS MYTABSTRIP TYPE TABSTRIP.
constants: c_usd type tcurr_curr value 'USD',
           c_eur type tcurr_curr value 'EUR'.

data vg_i       type i.
define mc_preenche_class.
  VG_I = VG_I + 1.
  CLEAR T_SORT.
  T_SORT-SPOS      = VG_I.
  T_SORT-FIELDNAME = &1.
  T_SORT-GROUP     = &2.
  IF &3 = 'D'.
    T_SORT-DOWN        = 'X'.
  ELSE.
    T_SORT-UP          = &3.
  ENDIF.
  T_SORT-SUBTOT    = &4.
  APPEND T_SORT.
end-of-definition.

***********************************************************************************
*Classe Seleção e Delete
***********************************************************************************
class cl_sel_del definition.

  public section.
    methods: del_estornados,
      sel_dados_83,
      sel_dados_59.

endclass.                    "CL_SEL_DEL DEFINITION

*----------------------------------------------------------------------*
*       CLASS CL_SEL_DEL IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sel_del implementation.

  method del_estornados.

*  Remove o Estorno da 83
    loop at it_zfit0083 assigning <wa_0083_del> where rev_trade cs 'E'.
      read table it_zfit0083 assigning <wa_zfit0083>
      with key rev_trade = <wa_0083_del>-rev_trade+1(1)
        trade_id         = <wa_0083_del>-trade_id.

      if sy-subrc is initial.
        <wa_0083_del>-rev_trade = 'W'.
        <wa_zfit0083>-rev_trade = 'W'.
      endif.

    endloop.

    delete it_zfit0083 where rev_trade eq 'W'.

  endmethod.                    "DEL_ESTORNADOS

  method sel_dados_83.

    free it_zfit0083.

    select *
      from zfit0083
      into table it_zfit0083
      where bukrs         eq wg_cadpro-bukrs
      and   date_period_1 gt wg_cadpro-dt_fechamento
      and   deal_type     eq '4'.


  endmethod.                    "SEL_DADOS_83

  method sel_dados_59.

    free it_zfit0059.

    select *
      from zfit0059
      into table it_zfit0059
      where mdo_tipo        eq wg_cadpro-cod_oper
      and   data_vencimento gt  wg_cadpro-dt_fechamento
      and   pfj_codigo      eq wg_cadpro-bukrs.

  endmethod.                    "SEL_DADOS_90

endclass.                    "CL_SEL_DEL IMPLEMENTATION

***********************************************************************************
*Classes Picture
***********************************************************************************
class cl_picture_click definition.

  public section.

    methods: picture_yclick for event picture_click of cl_gui_picture
      importing mouse_pos_x mouse_pos_y.

    methods: picture_yclick1 for event picture_click of cl_gui_picture
      importing mouse_pos_x mouse_pos_y.

endclass.                    "cl_picture_click DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_picture_click DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_picture_click implementation.

  method: picture_yclick.
    perform onclick. "write the action of click in this form.
  endmethod.                    "picture_yclick

  method: picture_yclick1.
    perform onclick1. "write the action of click in this form.
  endmethod.                    "picture_yclick
endclass.                    "CL_PICTURE_CLICK IMPLEMENTATION

data: obj_cl_picture_class type ref to cl_picture_click.
data: it_event_picture type table of cntl_simple_event,
      wa_event_picture type cntl_simple_event.
data: obj_sel_del type ref to cl_sel_del.

*&---------------------------------------------------------------------*
*&      Form  ONCLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form onclick.
  g_tab_tela-subtela   = '0210'.
endform.                    "ONCLICK

*&---------------------------------------------------------------------*
*&      Form  ONCLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form onclick1.
  g_tab_tela-subtela   = '0211'.
endform.                    "ONCLICK

***********************************************************************************
*Classes
***********************************************************************************

data : ty_toolbar type stb_button.

*Class definition for ALV toolbar
class:      lcl_alv_toolbar   definition deferred.

data: obg_toolbar          type ref to lcl_alv_toolbar.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class lcl_alv_toolbar definition.
  public section.
*Constructor
    methods: constructor
      importing io_alv_grid type ref to cl_gui_alv_grid,
*Event for toolbar
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.                    "lcl_alv_toolbar DEFINITION


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class lcl_alv_toolbar implementation.
  method constructor.
*   Create ALV toolbar manager instance
    create object c_alv_toolbarmanager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.                    "constructor

  method on_toolbar.
    data: wl_desactive.

    if wg_acao ne c_modif or g_tab_tela-subtela ne '0210'.
      wl_desactive = 1.
    endif.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.


    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-butn_type = 3.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    call method c_alv_toolbarmanager->reorganize
      exporting
        io_alv_toolbar = e_object.
  endmethod.                    "on_toolbar
  method handle_user_command.
    data: tl_itens_aux like table of tg_itens,
          wl_itens     like line of tg_itens,
          wl_lines     type sy-tabix.
    refresh: tl_itens_aux.


    case e_ucomm.
      when c_add.
        tl_itens_aux[] = tg_itens[].
        refresh: tg_itens.
        wl_lines = 1.
        loop at tl_itens_aux into wl_itens.
          append wl_itens to tg_itens.
          add 1 to wl_lines.
        endloop.
        clear: wl_itens.
        wl_itens-seq = wl_lines.
        append wl_itens to tg_itens.

        call method grid1->refresh_table_display
          exporting
            is_stable = wa_stable.
      when c_del.
        call method grid1->get_selected_cells
          importing
            et_cell = tg_selectedcell.

        loop at tg_selectedcell into wg_selectedcell.
          delete tg_itens index wg_selectedcell-row_id-index.
        endloop.

        call method grid1->refresh_table_display
          exporting
            is_stable = wa_stable.
    endcase.

  endmethod.                    "zm_handle_user_command

endclass.                    "lcl_alv_toolbar IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler definition.

  public section.
    class-methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed3 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed5 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed6 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed7 for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.


endclass.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler implementation.
* Método de  execução para Duplo-click
  method on_data_changed.

    data: ls_good         type lvc_s_modi,
          lv_value        type lvc_value,
          vl_value        type lvc_value,
          vl_nro_cto      type zfit0064-nro_cto,
          vl_tp_ajuste    type zfit0064-tp_ajuste,
          vl_vlr_aj_merc  type zfit0069-vlr_aj_merc,
          wa_zfit0214     type zfit0214,

          v_dias_corridos type zfit0060-dias_corridos,
          v_dt_base_bmf   type zfit0060-dt_base_bmf.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'DIAS_CORRIDOS'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      v_dt_base_bmf = wg_cadpro-dt_fechamento.
      v_dias_corridos = lv_value.
      add v_dias_corridos to v_dt_base_bmf.
      move v_dt_base_bmf to lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DT_BASE_BMF'
          i_value     = lv_value.

      perform calc_dias_uteis using wg_cadpro-dt_fechamento v_dt_base_bmf changing v_dias_uteis.

      move v_dias_uteis to lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DIAS_UTEIS'
          i_value     = lv_value.

    endloop.

    loop at er_data_changed->mt_good_cells
                                 into ls_good
                                 where fieldname = 'VLR_EXTRATO_BCO'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_EXTRATO_BCO'
          i_value     = lv_value.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'NRO_CTO'
        importing
          e_value     = vl_nro_cto.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
        importing
          e_value     = vl_tp_ajuste.

      clear wa_zfit0214-vlr_extrato_bco.
      clear vl_vlr_aj_merc.
      read table tg_itens3 into data(wg_itens3) with key nro_cto = vl_nro_cto.
      if sy-subrc = 0.
        vl_vlr_aj_merc = wg_itens3-vlr_pres_mtm.
        move-corresponding wg_itens3 to wa_zfit0214.
        wa_zfit0214-vlr_extrato_bco = lv_value.
        if wa_zfit0214-vlr_extrato_bco ne 0.
          modify zfit0214 from wa_zfit0214.
        else.
          delete zfit0214 from wa_zfit0214.
        endif.
      else.
        read table tg_itens5 into data(wg_itens5) with key nro_cto = vl_nro_cto.
        if sy-subrc = 0.
          vl_vlr_aj_merc = wg_itens5-vlr_aj_merc.
          move-corresponding wg_itens5 to wa_zfit0214.
          wa_zfit0214-vlr_extrato_bco = lv_value.
          if wa_zfit0214-vlr_extrato_bco ne 0.
            modify zfit0214 from wa_zfit0214.
          else.
            delete zfit0214 from wa_zfit0214.
          endif.
        else.
          read table tg_itens6 into data(wg_itens6) with key nro_cto = vl_nro_cto.
          if sy-subrc = 0.
            vl_vlr_aj_merc = wg_itens6-vlr_aj_merc.
            move-corresponding wg_itens6 to wa_zfit0214.
            wa_zfit0214-vlr_extrato_bco = lv_value.
            if wa_zfit0214-vlr_extrato_bco ne 0.
              modify zfit0214 from wa_zfit0214.
            else.
              delete zfit0214 from wa_zfit0214.
            endif.
          else.
            read table tg_itens7 into data(wg_itens7) with key nro_cto = vl_nro_cto.
            if sy-subrc = 0.
              vl_vlr_aj_merc = wg_itens7-vlr_aj_merc.
              move-corresponding wg_itens7 to wa_zfit0214.
              wa_zfit0214-vlr_extrato_bco = lv_value.
              if wa_zfit0214-vlr_extrato_bco ne 0.
                modify zfit0214 from wa_zfit0214.
              else.
                delete zfit0214 from wa_zfit0214.
              endif.
            endif.
          endif.
        endif.
      endif.

      if wa_zfit0214-vlr_extrato_bco ne 0.
        if wa_zfit0214-vlr_extrato_bco gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      elseif vl_vlr_aj_merc ne 0.
        if vl_vlr_aj_merc gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
          i_value     = lv_value.



    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed3.

    data: ls_good         type lvc_s_modi,
          lv_value        type lvc_value,
          vl_value        type lvc_value,
          vl_nro_cto      type zfit0064-nro_cto,
          vl_tp_ajuste    type zfit0064-tp_ajuste,
          vl_vlr_aj_merc  type zfit0069-vlr_aj_merc,
          wa_zfit0214     type zfit0214,

          v_dias_corridos type zfit0060-dias_corridos,
          v_dt_base_bmf   type zfit0060-dt_base_bmf.

    loop at er_data_changed->mt_good_cells
                                 into ls_good
                                 where fieldname = 'VLR_EXTRATO_BCO'.
      vg_extrato = 'X'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_EXTRATO_BCO'
          i_value     = lv_value.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'NRO_CTO'
        importing
          e_value     = vl_nro_cto.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
        importing
          e_value     = vl_tp_ajuste.

      clear wa_zfit0214-vlr_extrato_bco.
      clear vl_vlr_aj_merc.
      read table tg_itens3 into data(wg_itens3) index ls_good-row_id.
      if sy-subrc = 0.
        vl_vlr_aj_merc = wg_itens3-vlr_pres_mtm.
        move-corresponding wg_itens3 to wa_zfit0214.
        wa_zfit0214-dt_fechamento = wg_cadpro-dt_fechamento.
        wa_zfit0214-vlr_extrato_bco = lv_value.
        if wa_zfit0214-vlr_extrato_bco ne 0.
          modify zfit0214 from wa_zfit0214.
        else.
          delete zfit0214 from wa_zfit0214.
        endif.

      endif.

      if wa_zfit0214-vlr_extrato_bco ne 0.
        if wa_zfit0214-vlr_extrato_bco gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      elseif vl_vlr_aj_merc ne 0.
        if vl_vlr_aj_merc gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
          i_value     = lv_value.

      read table it_zfit0064 assigning field-symbol(<wa_zfit0064>) with key dt_fechamento  = wg_cadpro-dt_fechamento
                                                                            bukrs          = wg_itens3-bukrs
                                                                            nro_cto        = wg_itens3-nro_cto.
      if sy-subrc = 0.
        <wa_zfit0064>-tp_ajuste = lv_value.
        <wa_zfit0064>-vlr_extrato_bco = wa_zfit0214-vlr_extrato_bco.
      endif.


    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed5.

    data: ls_good         type lvc_s_modi,
          lv_value        type lvc_value,
          vl_value        type lvc_value,
          vl_nro_cto      type zfit0064-nro_cto,
          vl_tp_ajuste    type zfit0064-tp_ajuste,
          vl_vlr_aj_merc  type zfit0069-vlr_aj_merc,
          wa_zfit0214     type zfit0214,

          v_dias_corridos type zfit0060-dias_corridos,
          v_dt_base_bmf   type zfit0060-dt_base_bmf.

    loop at er_data_changed->mt_good_cells
                                 into ls_good
                                 where fieldname = 'VLR_EXTRATO_BCO'.
      vg_extrato = 'X'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_EXTRATO_BCO'
          i_value     = lv_value.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'NRO_CTO'
        importing
          e_value     = vl_nro_cto.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
        importing
          e_value     = vl_tp_ajuste.

      clear wa_zfit0214-vlr_extrato_bco.
      clear vl_vlr_aj_merc.
      read table tg_itens5 into data(wg_itens5) index ls_good-row_id.
      if sy-subrc = 0.
        vl_vlr_aj_merc = wg_itens5-vlr_aj_merc.
        move-corresponding wg_itens5 to wa_zfit0214.
        wa_zfit0214-dt_fechamento = wg_cadpro-dt_fechamento.
        wa_zfit0214-vlr_extrato_bco = lv_value.
        if wa_zfit0214-vlr_extrato_bco ne 0.
          modify zfit0214 from wa_zfit0214.
        else.
          delete zfit0214 from wa_zfit0214.
        endif.

      endif.

      if wa_zfit0214-vlr_extrato_bco ne 0.
        if wa_zfit0214-vlr_extrato_bco gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      elseif vl_vlr_aj_merc ne 0.
        if vl_vlr_aj_merc gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
          i_value     = lv_value.

      read table it_zfit0067 assigning field-symbol(<wa_zfit0067>) with key dt_fechamento  = wg_cadpro-dt_fechamento
                                                                            bukrs          = wg_itens5-bukrs
                                                                            nro_cto        = wg_itens5-nro_cto
                                                                            nro_par        = wg_itens5-nro_par.
      if sy-subrc = 0.
        <wa_zfit0067>-tp_ajuste = lv_value.
        <wa_zfit0067>-vlr_extrato_bco = wa_zfit0214-vlr_extrato_bco.
      endif.

    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed6.

    data: ls_good         type lvc_s_modi,
          lv_value        type lvc_value,
          vl_value        type lvc_value,
          vl_nro_cto      type zfit0064-nro_cto,
          vl_tp_ajuste    type zfit0064-tp_ajuste,
          vl_vlr_aj_merc  type zfit0069-vlr_aj_merc,
          wa_zfit0214     type zfit0214,

          v_dias_corridos type zfit0060-dias_corridos,
          v_dt_base_bmf   type zfit0060-dt_base_bmf.

    loop at er_data_changed->mt_good_cells
                                 into ls_good
                                 where fieldname = 'VLR_EXTRATO_BCO'.
      vg_extrato = 'X'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_EXTRATO_BCO'
          i_value     = lv_value.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'NRO_CTO'
        importing
          e_value     = vl_nro_cto.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
        importing
          e_value     = vl_tp_ajuste.

      clear wa_zfit0214-vlr_extrato_bco.
      clear vl_vlr_aj_merc.
      read table tg_itens6 into data(wg_itens6) index ls_good-row_id.
      if sy-subrc = 0.
        vl_vlr_aj_merc = wg_itens6-vlr_aj_merc.
        move-corresponding wg_itens6 to wa_zfit0214.
        wa_zfit0214-dt_fechamento = wg_cadpro-dt_fechamento.
        wa_zfit0214-vlr_extrato_bco = lv_value.
        if wa_zfit0214-vlr_extrato_bco ne 0.
          modify zfit0214 from wa_zfit0214.
        else.
          delete zfit0214 from wa_zfit0214.
        endif.

      endif.

      if wa_zfit0214-vlr_extrato_bco ne 0.
        if wa_zfit0214-vlr_extrato_bco gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      elseif vl_vlr_aj_merc ne 0.
        if vl_vlr_aj_merc gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
          i_value     = lv_value.

      "
      read table it_zfit0069 assigning field-symbol(<wa_zfit0069>) with key dt_fechamento  = wg_cadpro-dt_fechamento
                                                                            bukrs          = wg_itens6-bukrs
                                                                            nro_cto        = wg_itens6-nro_cto
                                                                            dt_fim_cto     = wg_itens6-dt_fim_cto.
      if sy-subrc = 0.
        <wa_zfit0069>-tp_ajuste = lv_value.
        <wa_zfit0069>-vlr_extrato_bco = wa_zfit0214-vlr_extrato_bco.
      endif.

    endloop.


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed7.

    data: ls_good         type lvc_s_modi,
          lv_value        type lvc_value,
          vl_value        type lvc_value,
          vl_nro_cto      type zfit0064-nro_cto,
          vl_tp_ajuste    type zfit0064-tp_ajuste,
          vl_vlr_aj_merc  type zfit0069-vlr_aj_merc,
          wa_zfit0214     type zfit0214,

          v_dias_corridos type zfit0060-dias_corridos,
          v_dt_base_bmf   type zfit0060-dt_base_bmf.

    loop at er_data_changed->mt_good_cells
                                 into ls_good
                                 where fieldname = 'VLR_EXTRATO_BCO'.
      vg_extrato = 'X'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_EXTRATO_BCO'
          i_value     = lv_value.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'NRO_CTO'
        importing
          e_value     = vl_nro_cto.

      call method er_data_changed->get_cell_value
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
        importing
          e_value     = vl_tp_ajuste.

      clear wa_zfit0214-vlr_extrato_bco.
      clear vl_vlr_aj_merc.
      read table tg_itens7 into data(wg_itens7) index ls_good-row_id.
      if sy-subrc = 0.
        vl_vlr_aj_merc = wg_itens7-vlr_aj_merc.
        move-corresponding wg_itens7 to wa_zfit0214.
        wa_zfit0214-dt_fechamento = wg_cadpro-dt_fechamento.
        wa_zfit0214-vlr_extrato_bco = lv_value.
        if wa_zfit0214-vlr_extrato_bco ne 0.
          modify zfit0214 from wa_zfit0214.
        else.
          delete zfit0214 from wa_zfit0214.
        endif.

      endif.

      if wa_zfit0214-vlr_extrato_bco ne 0.
        if wa_zfit0214-vlr_extrato_bco gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      elseif vl_vlr_aj_merc ne 0.
        if vl_vlr_aj_merc gt 0.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'ATIVO'.
          else.
            lv_value = 'ATIVOLP'.
          endif.
        else.
          if vl_tp_ajuste = 'ATIVO' or vl_tp_ajuste = 'PASSIVO'.
            lv_value = 'PASSIVO'.
          else.
            lv_value = 'PASSIVOLP'.
          endif.
        endif.
      endif.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_AJUSTE'
          i_value     = lv_value.

    endloop.

    read table it_zfit0070 assigning field-symbol(<wa_zfit0070>) with key dt_fechamento  = wg_cadpro-dt_fechamento
                                                                          bukrs          = wg_itens7-bukrs
                                                                          nro_cto        = wg_itens7-nro_cto
                                                                          nro_par        = wg_itens7-nro_par.
    if sy-subrc = 0.
      <wa_zfit0070>-tp_ajuste = lv_value.
      <wa_zfit0070>-vlr_extrato_bco = wa_zfit0214-vlr_extrato_bco.
    endif.

  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_finished.

*** Método de atualização de dados na Tela
    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.

    perform verifica_erros.

    call function 'Z_DOC_CHECK_NEW'
      exporting
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      importing
        e_messagem    = wg_mensagem
      tables
        it_msgs       = tg_msg_ret.

  endmethod.                    "on_data_changed_finisheD


endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION
