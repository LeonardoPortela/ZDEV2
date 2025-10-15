*&---------------------------------------------------------------------*
*& Report  ZFIY0032
*& Developer Enio Jesus
*& Data: 18.09.15
*&---------------------------------------------------------------------*

report  zfi0029.

tables: bkpf, bseg, icon.

types: begin of ty_saida,
check     type c,
status    type icon-id,
belnr     type bkpf-belnr,
bukrs     type bukrs,
gjahr     type gjahr,
budat     type budat,
hkont     type hkont,
bschl     type bschl,
prctr     type prctr,
shkzg     type shkzg,
gsber     type gsber,
dmbtr_brl type dmbtr,
dmbtr_usd type dmbtr,
tx_cambio type p length 9 decimals 5,
tx_ajuste type wkurs,
vlr_calc  type dmbtr,
vlr_ajus  type char20,
kunnr     type kunnr,
lifnr     type lifnr,
kostl     type kostl,
matnr     type matnr,
zfbdt     type dzfbdt,
zlspr     type bseg-zlspr, "Chv bloq pgto
zlsch     type bseg-zlsch, "Forma pgto
aufnr     type aufnr,
buzid     type buzid,
bldat     type bldat,
koart     type koart,
waers     type waers,
wrbtr     type wrbtr,
xblnr     type xblnr,
umskz     type umskz,
sgtxt     type sgtxt,
ebeln     type ebeln, "Pedido
ebelp     type ebelp, "Item
kidno     type kidno,
land1     type land1_gp,
blart     type blart,
stblg     type stblg,
tcode     type tcode,
objkey    type awkey,
zuonr     type dzuonr,
estorno   type c,
sequencia type c,
credito   type c,
estilo    type lvc_t_styl,
end of ty_saida.

data: obj_toolbar_manager type ref to cl_alv_grid_toolbar_manager,
      obj_custom_0100     type ref to cl_gui_custom_container,
      obj_alv_0100        type ref to cl_gui_alv_grid,
      gt_bseg             type table of bseg,
      gt_bkpf             type table of bkpf,
      gt_saida            type table of ty_saida,
      gt_fcat             type table of lvc_s_fcat,
      gt_zib_contabil     type table of zib_contabil,
      gt_bdc              type table of bdcdata,
      gt_estilo           type lvc_t_styl with header line,
      wl_bdc              type bdcdata,
      wl_bseg             type bseg,
      wl_bkpf             type bkpf,
      wl_saida            type ty_saida,
      wl_exclude          type ui_functions,
      wl_toolbar          type stb_button,
      wl_fcat             type lvc_s_fcat,
      wl_zib_contabil     type zib_contabil,
      wl_rseg             type rseg,
      wl_mseg             type mseg,
      wl_layout           type lvc_s_layo,
      wl_stable           type lvc_s_stbl,
      wl_stable_aux       type lvc_s_stbl,
      wl_tcurr            type tcurr,
      wl_ce4magi_acct     type ce4magi_acct,
      wl_estilo           type lvc_s_styl,
      wl_zib_contabil_err type zib_contabil_err,
      wl_zib_contabil_chv type zib_contabil_chv,
      opt                 type ctu_params,
      at_obj_key          type char20.

constants:
      c_x                 type char15 value 'X',
      c_n                 type char15 value 'N',
      c_s                 type char15 value 'S',
      c_f                 type char15 value 'F', "Lançamento finalizado
      c_usd               type waers  value 'USD',
      c_brl               type waers  value 'BRL',
      c_tipo_doc          type char15 value 'LM'.

selection-screen: begin of block b1 with frame title text-001.
select-options: s_bukrs for bkpf-bukrs,
                s_belnr for bkpf-belnr,
                s_blart for bkpf-blart,
                s_hkont for bseg-hkont,
                s_budat for bkpf-budat.
selection-screen: end of block b1.

start-of-selection.
  perform: seleciona_dados.

  check ( gt_saida is not initial ).
  call screen 0100.

end-of-selection.

  include zfi0029_class.
  include zfi0029_forms.
  include zfi0029_pbo.
