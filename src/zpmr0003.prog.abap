******************************************************************************
***                         AMAGGI IMPORTAÇÃO                              ***
******************************************************************************
*** Programa  : ZPMR0003                                                   ***
***                                                                        ***
*** Descrição : Acompanhamento de desvios dos planos de manutenção         ***
**              Especificação feita pelo analista Cleudo                   ***
**                                                                         ***
*** Objetivo  : Controlar execução de planos                               ***
***                                                                        ***
*** Versão    Autor               Data          Observações                ***
*** ------    ----------          ----------    --------------             ***
***                                                                        ***
******************************************************************************

report  zpmr0003.

tables: aufk, sscrfields, equz, mmpt, itob, *mmpt, t399w, *t399w, mpla, *mpla, mhio, *mhio, mpos, iflo.

*** Macros
***********************************************************
define mc_preenche_fieldcat.

  clear gv_fcat.
  gv_fcat-fieldname     = &1.
  gv_fcat-datatype      = &2.
  gv_fcat-coltext       = &3.
  gv_fcat-just          = &4.
  gv_fcat-key           = &5.
  gv_fcat-just          = &6.
  gv_fcat-edit_mask     = &7.
  gv_fcat-tabname       = &8.
  insert gv_fcat into table gt_fieldcatalog.



end-of-definition.

define mc_preenche_class.

  vg_i = vg_i + 1.
  clear it_sort.
  it_sort-spos      = vg_i.
  it_sort-fieldname = &1.
  it_sort-group     = &2.
  it_sort-up        = &3.
  it_sort-subtot    = &4.
  append it_sort.

end-of-definition.
* Definições para ALV
type-pools: kkblo.  "Tipos globais para ALV

*----------------------------------------------------------*
*           Definição de variáveis globais                 *
*----------------------------------------------------------*

data: v_tabix      type sy-tabix        ,  " guardar o indice
      v_permissao  type c,
      v_bukrs(50)  type c,
      v_werks(50)  type c,
      vg_horizonte type vimhis-horiz.

*---------------------------------------------------------*
*               Types                                     *
*---------------------------------------------------------*
types:
  begin of ty_equi,            " equi: Equipamento dados mestres
    pltxt type iflo-pltxt,     " Descrição do local
    equnr type equi-equnr,     " Nº equipamento
    aedat type equi-aedat,     " Data da última modificação
    eqtyp type equi-eqtyp,     " Categoria de equipamento
    eqart type equi-eqart,     " Tipo do objeto técnico
    objnr type equi-objnr,     " Nº objeto
    datbi type equz-datbi,     " Data de validade final
    iwerk type equz-iwerk,     " Centro de planejamento de manutenção
    tidnr type equz-tidnr,     " Nº identificação técnica
    eartx type t370k_t-eartx,  " Descrição do tipo equipamento
    typbz type equi-typbz,     " Denominação do tipo atribuído pelo fabricante
  end of ty_equi,

  begin of ty_equz,  " equz: Intervalo de tempo equipamento
    equnr type equz-equnr,  " Nº equipamento
    datbi type equz-datbi,  " Data de validade final
    iwerk type equz-iwerk,  " Centro de planejamento de manutenção
    tidnr type equz-tidnr,  " Nº identificação técnica
  end of ty_equz,

  begin of ty_t370k_t,       " Descrição do tipo do equipamento
    eqart type t370k_t-eqart ,   " código
    eartx type t370k_t-eartx ,   " descrição
  end of ty_t370k_t,

  begin of ty_relatorio,      " Dados do relatorio
    iwerk      type equz-iwerk,    " Centro
    equnr      type equz-equnr ,   " Codigo do equipamento
    shtxt      type itob-shtxt ,   " Denominação do objeto técnico
    eartx      type t370k_t-eartx, " Tipo de veículo
    warpl      type mpos-warpl,    " Plano de manutenção
    zykl1(8)   type c,             " Ciclo / offset de um pacote de manutenção
    pak_text   type mmpt-pak_text, " Texto p/o pacote ou ciclo de manutenção (tempo/rendimento)
    wptxt      type mpla-wptxt ,   " Descrição
    szaeh(8)   type c,             " Posição Contador
    rzaeh(8)   type c,             " Posição contador conclusão
    rzaeh_     type caufv-idat2,    " Posição contador conclusão
    szaeh_     type caufv-idat2,   " Posição Contador
    nzaeh_     type caufv-idat2,   " Posição contador conclusão
    totac(8)   type c,             " Posição Total Contador
    nzaeh(8)   type c,             " Próxima leitura planejada de contador
    uso(8)     type c,             " Utilização
    desvio(8)  type c,             " Desvio
    per_desv   type i,             " Desvio
    idat2      type caufv-idat2,   " Data Ultima_troca
    bautl      type mpos-bautl,    " Sistema
    cell_color type lvc_t_scol,    " Cor da Célula
    zeieh      type mmpt-zeieh,
    pltxt      type iflo-pltxt,
    eqktx      type eqkt-eqktx,
    ordem(4)   type c,
    w_ordem    type aufnr,
    modelo     type typbz,
    avenc,
    venc,
  end of ty_relatorio .

types: begin of ty_calc,
         abnum     type mhis-abnum,
         warpl     type mpla-warpl,
         nplda     type mhis-nplda,
         stadt     type mhis-stadt,
         lrmdt     type mhis-lrmdt,
         mptyp     type mpla-mptyp,
         zeieh     type mmpt-zeieh,
         zykl1     type mmpt-zykl1,
         zykl2(8)  type c,
         uso(8)    type c,
         desvio(8) type c,
         per_desv  type i,
       end of ty_calc.

types: begin of ty_viaufkst,
         bukrs  type viaufkst-bukrs,
         werks  type viaufkst-werks,
         warpl  type viaufkst-warpl,
         aufnr  type viaufkst-aufnr,
         qmnum  type viaufkst-qmnum,
         erdat  type viaufkst-erdat,
         objnr  type caufv-objnr,
         sttxt  type caufvd-sttxt,
         asttx  type caufvd-asttx,
         status type char10.
types: end of ty_viaufkst.


types: begin of ty_viqmel.
         include structure viqmel.
types:   sttxt  type caufvd-sttxt,
         asttx  type caufvd-asttx,
         status type char10.
types: end of ty_viqmel.


data: it_calc  type table of ty_calc with header line.

data: clicks type sy-tabix.

data: v_reinicio(8)  type c,
      v_reinicio1(8) type c,
      v_reinicio2(8) type c.

*----------------------------------------------------*
*                Tabelas Internas                    *
*----------------------------------------------------*
data:
  t_equi       type table of ty_equi       with header line,
  t_afih       type table of afih,
  t_equz       type table of ty_equz       with header line,
  t_t370k_t    type table of ty_t370k_t    with header line,
  t_relatorio  type table of ty_relatorio,
  t_relatorio1 type table of ty_relatorio,
  t_planos     type table of ty_relatorio,
  w_relatorio  type ty_relatorio,
  t_zeieh      type range of mmpt-zeieh.

data: t_ordem  type table of ty_viaufkst.
data: t_viqmel type table of ty_viqmel.
data: linha_selecionada type slis_selfield.
data: _exit             type c.

constants: yx  value 'X'.              "Flag X

*** Declaração de constantes
***********************************************************
constants: cc_a        type c value 'A',
           cc_x        type c value 'X',
           cc_i        type c value 'I',
           cc_1        type c value '1',
           cc_2        type c value '2',
           cc_spras(2) type c value 'PT',
           cc_m        type c value 'M',
           wc_upd      type c value 'U',    "indicator: Update
           wc_dele     type c value 'L'.    "indicator: delete

data: ti_linecolor  type slis_specialcol_alv   occurs 0 with header line,
      ti_listheader type slis_t_listheader,
      ti_fieldcat   type slis_t_fieldcat_alv            with header line,
      ti_sort       type slis_sortinfo_alv     occurs 0 with header line.

data: ok_code     type          sy-ucomm,
      return_code type          sy-subrc,
      gs_layout   type          lvc_s_layo,
      wa_color    type          lvc_s_scol,  " Cor para célula
      it_color    type table of lvc_s_scol,  " Cor para célula
      it_s_fcat   type lvc_t_fcat,
      pt_exclude  type          ui_functions. " internal table declaration to be passed.

data: l_flstr             like rihimrg-desic   .

data: t_equi_tabix like sy-index,
      t_equi_max   like sy-index,
      t_mpos_max   like sy-index,
      t_equz_max   like sy-index,
      t_jest_max   like sy-index,
      msg(60)      type c.

*--- strategy header
types: begin of ty_t351.
         include structure t351.
types:   ktext like t351t-ktext.
types: end of ty_t351.
types: wc_t_t351 type ty_t351 occurs 0.
data: g_w_t351        type ty_t351.

*--- data of packages (strategy)
types: begin of ty_package_data.
         include structure t351p as t351p renaming with suffix t351p.
         include structure t351x as t351x renaming with suffix t351x.
types: end of ty_package_data.
data: g_t_package_data
      type table of ty_package_data with header line.

*--- data (task list and strategy) of packages
types: begin of ty_package_data_st_tl.
         include structure plwp as plwp renaming with suffix plwp.
         include structure t351p as t351p renaming with suffix strat.
types:   plan         like mpos-warpl,
         item         like mpos-wapos,
         wppos        like mpos-wppos,
         ind_valid(1) type c.
types: end of ty_package_data_st_tl.

**--- package (task list and strategie) data
data: g_t_package_data_st_tl
      type table  of ty_package_data_st_tl with header line.

*---  Cycle definitions and MeasPoints (MMPT)
types: begin of ty_wmmpt.
         include structure mmpt.
         include structure mmpt_addition.
types: end   of ty_wmmpt.
types: wc_t_wmmpt type ty_wmmpt occurs 0.
data:  mmpt_tab       type ty_wmmpt occurs 0 with header line.

*--- maintenance item (MPOS)
types: begin of ty_wmpos.
         include structure mpos.
         include structure mpos_addition.
types: end   of ty_wmpos.
types: wc_t_wmpos type ty_wmpos occurs 0.
data: impos type ty_wmpos occurs 1 with header line.    "table


data: begin of index_mmpt occurs 20.
        include structure inxx.        "Manutenção Lista de Itens
data: end of index_mmpt.

types: begin of ty_mpos.
         include structure mpos.
types:   pltxt type iflo-pltxt.
types: end   of ty_mpos.
data: t_mpos type table of ty_mpos with header line.
data: t_eqkt       type table of eqkt with header line.

types: begin of ty_vimhis.
         include structure vimhis.
types:   readg like  imrg-readg.
types: end   of ty_vimhis.
data: t_vimhis type table of ty_vimhis with header line.

* Status
types: begin of ty_jest.          " Status da Ordem
         include structure jest.
types: end of ty_jest.
data: t_jest type table of ty_jest  with header line.

data: begin of wa_impt.
        include structure impt.
data: end of wa_impt.

data: begin of wa_imrg.
        include structure imrg.
data: end of wa_imrg.

data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.

data: fltp_char     type imrc_totac,
      point_txt(40) type c,
      anz_verpak    like t351p-zaehl,
      sttag         type sy-datum,
      mmpt_max      like sy-index,
      gv_rc(2)      type c,
      gv_uso        type imrc_readg,
      gv_desvio     type imrc_readg.

* Fieldcatalog
data: gt_fieldcatalog type lvc_t_fcat,
      gv_fcat         like line of gt_fieldcatalog,
      ls_stable       type lvc_s_stbl.

*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*
* Control
data: g_container        type scrfname value 'ALV_CONTAINER',
      g_custom_container type ref to cl_gui_custom_container,
      r_grid             type ref to cl_gui_alv_grid.

data: obj_custom_0110 type ref to cl_gui_custom_container,
      obj_alv_0110    type ref to cl_gui_alv_grid.

* Codigos para chamada da transação IP10.
class lcl_events_handler definition deferred.

data: r_event_handler type ref to lcl_events_handler,
      i_selected_rows type lvc_t_row,                "Linhas selecionadas
      w_selected_rows type lvc_s_row.                "Colunas Selecionadas

data: p_resp, check, p_erro(1).
data: ti_bdcdata type standard table of bdcdata,
      wa_bdcdata like line of ti_bdcdata.

data:et_return type table of bapiret2 with header line,
     ls_return type bapiret2.

data: ord_atv type char1.

*---------------------------------------------------------------------*
*       CLASS lcl_events_handler DEFINITION
*---------------------------------------------------------------------*
class lcl_events_handler definition.
  public section.
    class-methods
      handle_double_click
        for event double_click of cl_gui_alv_grid
        importing e_row e_column.
endclass.                    "lcl_events_handler DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_events_handler IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_events_handler implementation.
*---Metodo para double_click.

  method handle_double_click.

    check e_row-rowtype(1) eq space.
    perform sel_ordem  using e_row e_column-fieldname.

  endmethod. " HANDLE_DOUBLE_CLICK
endclass.                    "lcl_events_handler IMPLEMENTATION



*----------------------------------------------------*
*                Parâmetros de Seleção               *
*----------------------------------------------------*
**  Begin of    #101527  FF  01.03.2023
selection-screen: begin of block b5 with frame title text-003.

  selection-screen begin of line.

    selection-screen comment 1(7) text-a01 for field p_avenc.
    parameters: p_avenc as checkbox .

    selection-screen comment 16(7) text-a02 for field p_venc.
    parameters: p_venc as checkbox.

  selection-screen end of line.

selection-screen: end of block b5.
** End of FF  01.03.2023
selection-screen: begin of block b1 with frame title text-004.
  selection-screen begin of line.

*SELECTION-SCREEN POSITION 1.
    parameter: p_ho radiobutton group b1.
    selection-screen comment 2(7) text-005 for field p_ho.
    selection-screen position 11.

    parameter: p_dt radiobutton group b1 default 'X'.
    selection-screen comment 14(10) text-006 for field p_dt.
    selection-screen position 25.

  selection-screen end of line.
selection-screen: end of block b1.

selection-screen: begin of block b2 with frame title text-001.
  select-options:
    s_mptyp for mpla-mptyp,
    s_equnr for itob-equnr,
    s_tplnr for iflo-tplnr,
    s_eqart for itob-eqart,
    s_bautl for mpos-bautl,
    s_tipoo for mpos-auart.
selection-screen: end of block b2.

selection-screen: begin of block b3 with frame title text-002.
  select-options:
  s_iwerk for itob-iwerk obligatory default '*',
  s_kostl for itob-kostl,
  s_warpl for mpos-warpl.
selection-screen: end of block b3.

selection-screen: begin of block b4 with frame title text-002.
  select-options:
  s_aufnr for aufk-aufnr.
selection-screen: end of block b4.

**FF - inicio
selection-screen: begin of block b6 with frame title text-007.
  parameter: p_equi as checkbox.
selection-screen: end of block b6.
**FF - fim


selection-screen function key 1.

initialization.
  sscrfields-functxt_01 = 'Listar ordem'.

at selection-screen. "PAI
  case sscrfields-ucomm. "pushbutton pressed
    when 'FC01'.
*      MESSAGE 'teste' TYPE 'S'.
*AT SELECTION-SCREEN OUTPUT.
      clear: ord_atv.
      loop at screen.
        case screen-name.
          when 'S_AUFNR-LOW' or 'S_AUFNR-HIGH'.
            screen-active = '1'.         "Hide parameters     "n921165
            ord_atv = '1'.
            modify screen.
        endcase.
      endloop.
  endcase.

*Classe para executar a lista de ordem.
class zcl_lis_ordem definition.
  public section.

    class-methods: zlist_ordem.

    class-methods: zshdb_ordem importing aufnr type aufnr.

    class-methods: selec_ordem importing warpl type warpl equnr type equnr
                               exporting aufnr type aufnr.



endclass.

class zcl_lis_ordem implementation.

  method selec_ordem.
    data(w_warpl) = warpl.
    clear: aufnr.
    data: r_stat type range of stat.
    data: t_afih type table of afih.

    data(aber) = 'I0001'.
    data(lib) =  'I0002'.

    append value #( sign = 'I'  option = 'EQ' low = aber  )  to r_stat.
    append value #( sign = 'I'  option = 'EQ' low = lib   )  to r_stat.

    if equnr is not initial.
      select *
      from afih as a
        inner join aufk as b on b~aufnr eq a~aufnr
      into corresponding fields of table t_afih
        where warpl eq w_warpl
          and equnr eq equnr
        and ( exists ( select * from jest
                       where objnr eq b~objnr
                         and inact ne abap_true
                         and stat  in ( aber, lib ) ) ).
    else.

      select *
      from afih as a
        inner join aufk as b on b~aufnr eq a~aufnr
      into corresponding fields of table t_afih
        where warpl eq w_warpl
        and ( exists ( select * from jest
                       where objnr eq b~objnr
                         and inact ne abap_true
                         and stat  in ( aber, lib ) ) ).

    endif.


    check t_afih is not initial.
    sort t_afih descending by aufnr equnr.

    read table t_afih into data(w_afih) index 1.
    if sy-subrc eq 0.
      aufnr = |{ w_afih-aufnr alpha = out }|.
    endif.

    clear: w_afih.
    free: t_afih.
  endmethod.

  method zlist_ordem.

    select *
    from aufk
    into table @data(t_aufnr)
      where aufnr in @s_aufnr.

    check t_aufnr is not initial.

    loop at t_aufnr assigning field-symbol(<_aufnr>).
      call method zshdb_ordem
        exporting
          aufnr = <_aufnr>-aufnr.
    endloop.


*    IF ( ET_RETURN[] IS NOT INITIAL ).
**      DELETE T_ORDEM WHERE WARPL NE WA_PLANOS-WARPL.
*
*      DATA(TL_FIELDCAT) = VALUE SLIS_T_FIELDCAT_ALV(
*
*      ( FIELDNAME = 'Mensagem  '        SELTEXT_M = 'Status  '  OUTPUTLEN = '10' ) ).
*
*      CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
*        EXPORTING
*          I_TITLE     = 'Status do processamento'
*          I_SELECTION = 'X'
*          I_TABNAME   = 'ET_RETURN'
*          I_ZEBRA     = 'X'
*          IT_FIELDCAT = TL_FIELDCAT
*        IMPORTING
*          ES_SELFIELD = LINHA_SELECIONADA
*          E_EXIT      = _EXIT
*        TABLES
*          T_OUTTAB    = ET_RETURN.
*    ENDIF.


  endmethod.

  method zshdb_ordem.

*  7007660, 61, 62

    free ti_bdcdata[].
    free it_msg[].

    perform f_bdc_data using:
  '        '  '    '  'T'     'IW32      '    '                                                               ',
  'SAPLCOIH'  '0101'  'X'     '            '    '                                                               ',
  '        '  '    '  ' '      'BDC_CURSOR   '    'CAUFVD-AUFNR                                                   ',
  '        '  '    '  ' '      'BDC_OKCODE   '    '/00                                                            ',
  '        '  '    '  ' '     'CAUFVD-AUFNR'    aufnr,
  'SAPLCOIH'  '3000'  'X'     '            '    '                                                               ',
  '        '  '    '  ' '      'BDC_OKCODE   '    '=BABL                                                          ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                3001SUB_ALL            ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                1100SUB_LEVEL          ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                1102SUB_KOPF           ',
*  '        '  '    '  ' '      'BDC_CURSOR  '    'CAUFVD-KTEXT                                                   ',
*  '        '  '    '  ' '     'CAUFVD-KTEXT'    'teste                                                          ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                1105SUB_BTN            ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                1104SUB_TEXT           ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                1120SUB_AUFTRAG        ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLIPAR                                0415SUB_ADRESSE        ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLIPAR                                0415SUB_ADDR_PM        ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                0154HEADER             ',
*  '        '  '    '  ' '     'CAUFVD-INGPR'    'IND                                                            ',
*  '        '  '    '  ' '     'CAUFVD-VAPLZ'    'ENEGER00                                                       ',
*  '        '  '    '  ' '     'CAUFVD-VAWRK'    '2502                                                           ',
*  '        '  '    '  ' '     'CAUFVD-ILART'    'Z01                                                            ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                0153MAINORDER          ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                0157PARTNER            ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                7400SUB_PM_ADDR        ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                7402SUB_PM_ADDR_BTN    ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                7300TERM               ',
*  '        '  '    '  ' '     'CAUFVD-GSTRP'    '18.06.2019                                                     ',
*  '        '  '    '  ' '     'CAUFVD-PRIOK'    '3                                                              ',
*  '        '  '    '  ' '     'CAUFVD-GLTRP'    '18.06.2019                                                     ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                7301SUB_BTN            ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                7310SUB_ADD            ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                7100OBJECT             ',
*  '        '  '    '  ' '     'CAUFVD-TPLNR'    'MAGI.025.2502.CF01.SEV01.VAL11                                 ',
*  '        '  '    '  ' '     'CAUFVD-EQUNR'    '601326                                                         ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                0815NOTIFICATION_DATA  ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOI0                                0310AVO                ',
*  '        '  '    '  ' '      'AFVGD-LTXA1 '    'teste                                                          ',
*  '        '  '    '  ' '      'AFVGD-INDET '    '1                                                              ',
*  '        '  '    '  ' '      'AFVGD-ARBPL '    'ENEGER00                                                       ',
*  '        '  '    '  ' '      'AFVGD-WERKS '    '2502                                                           ',
*  '        '  '    '  ' '      'AFVGD-STEUS '    'PM01                                                           ',
*  '        '  '    '  ' '      'AFVGD-ARBEH '    'H                                                              ',
*  '        '  '    '  ' '     'AFVGD-DAUNE '    'H                                  ',
  '        '  '    '  ' '      'BDC_SUBSCR   '    'SAPLCOIH                                0153SUB_SERVICE        ',
  'SAPLSPO1'  '0100'  'X'     '            '    '                                                               ' ,
  '        '  '    '  ' '     'BDC_OKCODE  '    '=YES                                 '.

    clear p_erro.
    perform zf_call_transaction using 'IW32' changing p_erro.

    if p_erro is initial.
      clear ls_return.
      ls_return-message = aufnr && ' - executado com sucesso'.
    else.
      clear ls_return.
      ls_return-message = 'Erro ao executar a ordem' && '-' && aufnr.
    endif.

    append ls_return to et_return.
  endmethod.
endclass.


*--------------------------------------------------------------------
* s t a r t - o f - s e l e c t i o n.
*--------------------------------------------------------------------
start-of-selection.

*  DATA: ZCL_OBJ TYPE REF TO ZCL_LIS_ORDEM.
*  CREATE OBJECT ZCL_OBJ.

  if s_aufnr is not initial.
    call method zcl_lis_ordem=>zlist_ordem.
  else.

    perform seleciona_dados.
    if t_relatorio is not initial.
      call screen 100.
    endif.
  endif.

end-of-selection.

at selection-screen on value-request for s_bautl-low.
  perform buscar_sistema.

at selection-screen on value-request for s_bautl-high.
  perform buscar_sistema.

at selection-screen output. "PAI

  loop at screen.
    case screen-name.
      when '%B002023_BLOCK_1000' or '%_S_AUFNR_%_APP_%-TEXT'
        or '%_S_AUFNR_%_APP_%-OPTI_PUSH' or 'S_AUFNR-LOW'
        or '%_S_AUFNR_%_APP_%-TO_TEXT' or 'S_AUFNR-HIGH'
        or '%_S_AUFNR_%_APP_%-VALU_PUSH'.


        if ord_atv eq 1.
          screen-active = '1'.         "Hide parameters     "n921165
        else.
          screen-active = '0'.         "Hide parameters     "n921165
        endif.
        modify screen.
    endcase.
  endloop.


*&---------------------------------------------------------------------*
*&      Form  buscar_sistema
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form buscar_sistema.
  data: tl_return_tab type table of ddshretval with header line,
        tl_dselc      type table of dselc      with header line.

  data: begin of tl_temp occurs 0,
          bautl type mpos-bautl,
          pstxt type mpos-pstxt,
        end of tl_temp.

  select distinct *
    from mpos
    into corresponding fields of table tl_temp
  where equnr in s_equnr.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'BAUTL'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'MPOS-BAUTL'
      value_org       = 'S'
    tables
      value_tab       = tl_temp
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
endform.                    "buscar_sistema

*&---------------------------------------------------------------------*
*&      module  user_command_0100  input
*&---------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK' or 'EXIT'.
*      CALL METHOD G_CUSTOM_CONTAINER->FREE.
      leave to screen 0.
    when 'CANCEL'.
*      CALL METHOD G_CUSTOM_CONTAINER->FREE.
      leave program .
    when 'REFRESH'.

**  Begin of    #101527  FF  01.03.2023
      perform seleciona_dados.
** End of FF  01.03.2023

      call method obj_alv_0110->refresh_table_display
        exporting
          is_stable = ls_stable
        exceptions
          finished  = 1
          others    = 2.



  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      module  status_0100  output
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'S100'.
  set titlebar 'T100'.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_objects output.
** create objects

  if obj_custom_0110 is initial.


*    MC_PREENCHE_FIELDCAT:

    perform it_catalog using:


*  1  'TIDNR'     ' '  'C'  'CHAR'  ' '  ' ' 'Id.Tec'         'X'  ' '  ' '  ' '  ' '  ' ',
   2  'IWERK'     ' '  'C'  'CHAR'  ' '  ' ' 'Centro'         'X'  ' '  ' '  ' '  ' '  ' ',
   3  'PLTXT'     ' '  'C'  'CHAR'  ' '  ' ' 'Local Instal'   ' '  ' '  ' '  ' '  ' '  ' ',
   4  'EQUNR'     ' '  'C'  'CHAR'  ' '  ' ' 'Equipamento'    ' '  ' '  ' '  ' '  'X'  ' ',
   5  'SHTXT'     ' '  'C'  'CHAR'  ' '  ' ' 'Denominação'    ' '  ' '  ' '  ' '  ' '  ' ',
   6  'EARTX'     ' '  'C'  'CHAR'  ' '  ' ' 'Tipo'           ' '  ' '  ' '  ' '  ' '  ' ',
   7  'WARPL'     ' '  'C'  'CHAR'  ' '  ' ' 'Plano'          ' '  ' '  ' '  ' '  'X'  ' ',
   8  'BAUTL'     ' '  'C'  'CHAR'  ' '  ' ' 'Sistema'        ' '  ' '  ' '  ' '  ' '  ' ',
   9  'WPTXT'     ' '  'C'  'CHAR'  ' '  ' ' 'Texto do plano' ' '  ' '  ' '  ' '  ' '  ' ',
   10 'PAK_TEXT'  ' '  'C'  'CHAR'  ' '  ' ' 'Texto do Ciclo' ' '  ' '  ' '  ' '  ' '  ' ',
   11 'ZYKL1'     ' '  '15' 'CURR'  ' '  ' ' 'Ciclo'          ' '  ' '  ' '  ' '  ' '  ' ',
   12 'USO'       ' '  '15' 'CURR'  ' '  ' ' 'Uso'            ' '  ' '  ' '  ' '  ' '  ' ',
   13 'DESVIO'    ' '  '15' 'CURR'  ' '  ' ' 'Desvio'         ' '  ' '  ' '  ' '  ' '  ' ',
   14 'W_ORDEM'   ' '  '08' 'CHAR'  ' '  ' ' 'Ordem atual'          ' '  ' '  ' '  ' '  ' '  ' ',
   15 'ORDEM'     ' '  '05' 'CHAR'  ' '  ' ' 'Hist.ordem'     ' '  ' '  ' '  ' '  'X'  ' ',
   16 'PER_DESV'  ' '  '15' 'CURR'  ' '  ' ' '% Desvio'       ' '  ' '  ' '  ' '  ' '  ' ',
   17 'RZAEH'     ' '  'C'  'CHAR'  ' '  ' ' 'Ult.Exec.Pl.'   ' '  ' '  ' '  ' '  ' '  ' '.


    if p_dt is not initial.
      perform  it_catalog  using:
      17 'NZAEH_'   ' '   'C' 'DATS'  ' '  ' ' 'Proxima'     ' '  ' '  ' '  ' ' ' '  ' '.
    else.
      perform  it_catalog  using:
      18 'NZAEH_'   ' '   'C' 'CHAR'  ' '  ' ' 'Proxima'     ' '  ' '  ' '  ' ' ' '  ' ',
      19 'TOTAC'    ' '   'C' 'CHAR'  ' '  ' ' 'Pos.Total'   ' '  ' '  ' '  ' ' ' '  ' '.
    endif.

    perform  it_catalog  using:
    20 'IDAT2'    ' '   'C' 'DATS'  ' '  ' ' 'Conclusão'     ' '  ' '  ' '  ' ' ' '  ' '.

    if p_dt is not initial.
      perform  it_catalog  using:
      21 'SZAEH_'   ' '   'C' 'DATS'  ' '  ' ' 'Reinicio Pl' ' ' ' '  ' '  ' '  ' ' ' '.
    else.
      perform  it_catalog  using:
      22 'SZAEH'    ' '   'C' 'CHAR'  ' '  ' ' 'Reinicio Pl' ' ' ' '  ' '  ' ' ' '  ' '.
    endif.
**  Begin of    #101527  FF  01.03.2023
    perform  it_catalog  using:
    23 'MODELO'    ' '   '20' 'CHAR'  ' '  ' ' 'Modelo' ' ' ' '  ' '  ' ' ' '  ' '.
** End of FF  01.03.2023
    gs_layout-ctab_fname     = 'CELL_COLOR'.
    gs_layout-zebra          = 'X'.
    gs_layout-sel_mode       = 'A'.
    gs_layout-cwidth_opt     = 'X'.     "  Otimizar colunas na tela

    perform exclui_tb_functions changing pt_exclude.

    create object obj_custom_0110
      exporting
        container_name              = 'ALV_CONTAINER'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    create object obj_alv_0110
      exporting
        i_parent = obj_custom_0110.


* Link used Events and Event Handler Methods
*    CREATE OBJECT R_EVENT_HANDLER.
    set handler: lcl_events_handler=>handle_double_click for obj_alv_0110.

* load data into the grid and display them
    call method obj_alv_0110->set_table_for_first_display
      exporting
        is_layout            = gs_layout
        it_toolbar_excluding = pt_exclude        "excluding toolbar functions
      changing
        it_fieldcatalog      = it_s_fcat
        it_outtab            = t_relatorio.
  endif.

  call method obj_alv_0110->refresh_table_display
    exporting
      is_stable = ls_stable
    exceptions
      finished  = 1
      others    = 2.

endmodule.                 " CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*&      form  load_dados_na_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form load_dados_na_grid.

  perform prepara_relatorio.
  perform popula_fieldcatalog.

  gs_layout-ctab_fname     = 'CELL_COLOR'.
  gs_layout-zebra          = 'X'.
  gs_layout-sel_mode       = 'A'.
  gs_layout-cwidth_opt     = 'X'.     "  Otimizar colunas na tela

  perform exclui_tb_functions changing pt_exclude.

* Link used Events and Event Handler Methods
  create object r_event_handler.
  set handler r_event_handler->handle_double_click for r_grid.

* load data into the grid and display them
  call method r_grid->set_table_for_first_display
    exporting
      is_layout            = gs_layout
      it_toolbar_excluding = pt_exclude        "excluding toolbar functions
    changing
      it_fieldcatalog      = gt_fieldcatalog
      it_outtab            = t_relatorio.

endform.                    " load_dados_na_grid

*&---------------------------------------------------------------------*
*&      Form  exclui_tb_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_EXCLUDE text
*----------------------------------------------------------------------*
form exclui_tb_functions changing pt_exclude type ui_functions.

  append cl_gui_alv_grid=>mc_fc_subtot      to pt_exclude.
  append cl_gui_alv_grid=>mc_fc_sum         to pt_exclude.
  append cl_gui_alv_grid=>mc_mb_subtot      to pt_exclude.
  append cl_gui_alv_grid=>mc_mb_sum         to pt_exclude.
  append cl_gui_alv_grid=>mc_fc_info        to pt_exclude.
  append cl_gui_alv_grid=>mc_fc_graph       to pt_exclude.
  append cl_gui_alv_grid=>mc_fc_print       to pt_exclude.


endform.                    "exclui_tb_functions

*&---------------------------------------------------------------------*
*&      Form  popula_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form popula_fieldcatalog.

  mc_preenche_fieldcat:

*      1           2         3             4    5    6    7    8
*   'TIDNR'       'CHAR' 'Id.Tec'         'C'  'X'  ' '  ' '  ' ',
    'IWERK'       'CHAR' 'Centro'         'C'  'X'  ' '  ' '  ' ',
    'PLTXT'       'CHAR' 'Local Instal'   'C'  ' '  ' '  ' '  ' ',
    'EQUNR'       'CHAR' 'Equipamento'    'C'  ' '  ' '  ' '  ' ',
    'SHTXT'       'CHAR' 'Denominação'    'C'  ' '  'L'  ' '  ' ',
    'EARTX'       'CHAR' 'Tipo'           'C'  ' '  'L'  ' '  ' ',
    'WARPL'       'CHAR' 'Plano'          'C'  ' '  ' '  ' '  ' ',
    'BAUTL'       'CHAR' 'Sistema'        'C'  ' '  'L'  ' '  ' ',
    'WPTXT'       'CHAR' 'Texto do plano' 'C'  ' '  'L'  ' '  ' ',
    'PAK_TEXT'    'CHAR' 'Texto do Ciclo' 'C'  ' '  'L'  ' '  ' ',
    'ZYKL1'       'CURR' 'Ciclo'          '15' ' '  ' '  ' '  ' ',
    'USO'         'CURR' 'Uso'            '15' ' '  ' '  ' '  ' ',
    'DESVIO'      'CURR' 'Desvio'         '15' ' '  ' '  ' '  ' ',
    'ORDEM'       'CHAR' 'Ordem     '     '05' ' '  ' '  ' '  ' ',
    'PER_DESV'    'CURR' '% Desvio'       '15' ' '  ' '  ' '  ' ',
    'RZAEH'       'CHAR' 'Ult.Exec.Pl.'   'C'  ' '  'L'  ' '  ' '.


  if p_dt is not initial.
*    MC_PREENCHE_FIELDCAT: 'NZAEH'       'CHAR' 'Proxima'        'C'  ' '  ' '  '__/__/____' ' '.
    mc_preenche_fieldcat: 'NZAEH_'       'DATS' 'Proxima'        'C'  ' '  ' '  ' ' ' '.
  else.
    mc_preenche_fieldcat: 'NZAEH'        'CHAR' 'Proxima'        'C'  ' '  ' '  ' ' ' ',
                          'TOTAC'        'CHAR' 'Pos.Total'      'C'  ' '  ' '  ' ' ' '.
  endif.

  mc_preenche_fieldcat:
*      'IDAT2'       'DATS' 'Conclusão'      'C'  ' '  ' '  '__/__/____' ' '.
       'IDAT2'       'DATS' 'Conclusão'      'C'  ' '  ' '  ' ' ' '.

  if p_dt is not initial.
*   MC_PREENCHE_FIELDCAT:'SZAEH'        'CHAR' 'Reinicio Pl.'   'C'  ' '  ' '  '__/__/____' ' '.
    mc_preenche_fieldcat:'SZAEH_'       'DATS' 'Reinicio Pl.'   'C'  ' '  ' '  ' ' ' '.
  else.
    mc_preenche_fieldcat:'SZAEH'       'CHAR' 'Reinicio Pl.'   'C'  ' '  ' '  ' ' ' '.
  endif.

  read table gt_fieldcatalog with key fieldname = 'EQUNR' into gv_fcat.
  gv_fcat-no_zero = 'X'.
  modify  gt_fieldcatalog from gv_fcat index sy-tabix transporting no_zero.
  read table gt_fieldcatalog with key fieldname = 'WARPL' into gv_fcat.
  gv_fcat-no_zero = 'X'.
  modify  gt_fieldcatalog from gv_fcat index sy-tabix transporting no_zero.

endform.                    "popula_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_dados .

  data: it_abastec type range of ztparam-zval,
        wa_abastec like line of it_abastec.
  data: it_ztparam type standard table of ztparam,
        wa_ztparam type ztparam.

  refresh: t_equi ,
           t_equz ,
           t_relatorio.
  clear:   w_relatorio, return_code.

  if s_equnr is initial and
     s_eqart is initial and
     s_iwerk is initial and
     s_tipoo is initial.
    message 'Nenhum Parametro de seleção foi informado.' type 'I'.
    return_code = 4.
    exit.
  endif.

  t_zeieh = value #(
                      ( sign = 'I' option = 'EQ' low = 'H'  )
                      ( sign = 'I' option = 'EQ' low = 'KM' )
                    ).

  " Se equipamento ou tipo do objeto técnico for preenchido.
*  IF NOT ( S_EQUNR IS INITIAL )
*  OR NOT ( S_EQART IS INITIAL )
*  OR NOT ( S_TPLNR IS INITIAL )
*  OR NOT ( S_KOSTL IS INITIAL ).

  select c~bukrs a~iwerk f~pltxt a~warpl d~wptxt a~pstxt a~bautl a~iloan a~equnr a~plnnr a~plnal a~inact a~ersdt a~status d~mptyp d~objnr a~wstra b~name1"E~EQKTX" B~NAME1
 from mpla as d
 inner join mpos  as a on a~warpl = d~warpl
 inner join t001w as b on b~werks = a~iwerk
 inner join iloa  as c on c~iloan = a~iloan
 inner join iflo  as f on f~tplnr = c~tplnr
*  INNER JOIN EQKT  AS E ON E~EQUNR = A~EQUNR
 into corresponding fields of table t_mpos
*  FOR ALL ENTRIES IN T_MPLA
 where bautl in s_bautl
 and a~iwerk in s_iwerk
*   AND D~WARPL IN S_WARPL
*   AND C~BUKRS IN S_BUKRS
*   AND D~MPTYP IN S_MPTYP
 and a~equnr in s_equnr
 and a~warpl in s_warpl
 and c~kostl in s_kostl
 and f~tplnr in s_tplnr
 and a~auart in s_tipoo
 and d~mptyp in s_mptyp
  and a~inact  eq abap_false.
  sort t_mpos ascending by warpl iwerk.

  loop at t_mpos.
    if t_mpos-warpl is not initial and t_mpos-wstra is not initial.
      delete t_mpos index sy-tabix.
    endif.
  endloop.

  select *
  from eqkt
  into table t_eqkt
  for all entries in t_mpos
  where equnr eq t_mpos-equnr.

  select equi~equnr equi~aedat equi~eqtyp equi~eqart equi~objnr equi~typbz
  from equi as equi
  inner join v_equi as v_equi on v_equi~equnr eq equi~equnr
  into corresponding fields of table t_equi
    for all entries in t_mpos
  where equi~equnr eq t_mpos-equnr
*    AND  EQUI~EQUNR IN S_EQUNR
  and  equi~eqart in s_eqart
  and  v_equi~swerk in s_iwerk
  and  v_equi~datbi = '99991231'.

*
*    SELECT IFLO~PLTXT EQUI~EQUNR EQUI~AEDAT EQUI~EQTYP EQUI~EQART EQUI~OBJNR
*    FROM IFLO AS IFLO
*    INNER JOIN V_FLEET_IFLOS AS IFLOS ON IFLOS~TPLNR EQ IFLO~TPLNR
*    INNER JOIN EQUI          AS EQUI   ON EQUI~EQUNR   EQ IFLOS~EQUNR
*    INNER JOIN V_EQUI        AS V_EQUI ON V_EQUI~EQUNR EQ EQUI~EQUNR
*    INTO CORRESPONDING FIELDS OF TABLE T_EQUI
*    WHERE EQUI~EQUNR IN S_EQUNR
*     AND  EQUI~EQART IN S_EQART
*     AND  IFLO~TPLNR IN S_TPLNR
*     AND  IFLO~KOSTL IN S_KOSTL
*     AND  V_EQUI~SWERK IN S_IWERK
*     AND  V_EQUI~DATBI = '99991231'.

*    IF SY-SUBRC <> 0.
*      IF ( S_EQUNR-LOW IS NOT INITIAL ) AND ( S_EQUNR-HIGH IS NOT INITIAL ).
*        CONCATENATE 'Equipamento ' S_EQUNR-LOW 'á' S_EQUNR-HIGH 'não encontrado.' INTO MSG SEPARATED BY SPACE.
*      ELSEIF ( S_EQUNR-LOW IS NOT INITIAL ).
*        CONCATENATE 'Equipamento ' S_EQUNR-LOW 'não encontrado.' INTO MSG SEPARATED BY SPACE.
*      ENDIF.
*
*      MESSAGE MSG TYPE 'I'.
*      RETURN_CODE = 4.
*      EXIT.

*    ENDIF.

  select *
    from ztparam
    into table it_ztparam
    where param eq 'TP_OBJ'
  and abastec eq 'X'.

  if it_ztparam is not initial.
    loop at it_ztparam into wa_ztparam.
      wa_abastec-sign = 'I'.
      wa_abastec-option = 'EQ'.
      wa_abastec-low = wa_ztparam-zval.
      append wa_abastec to it_abastec.
    endloop.
  endif.

  if t_equi is not initial.
    delete t_equi where eqtyp not in it_abastec.



    select v_equi~equnr v_equi~datbi v_equi~iwerk v_equi~tidnr
      into corresponding fields of table t_equz
      from v_equi
      for all entries in t_equi
      where   equnr       eq t_equi-equnr
    and     v_equi~datbi = '99991231'.

*     deleta equipamentos duplicados
    sort t_equz by iwerk equnr ascending.

    delete adjacent duplicates from t_equz comparing equnr.
  endif.

  " Senão centro ou tipo de ordem for preenchido.
*  ELSEIF NOT ( S_IWERK[] IS INITIAL ) OR ( S_TIPOO IS NOT INITIAL )  . "Modificação CS2016001626

*  SELECT V_EQUI~EQUNR V_EQUI~DATBI V_EQUI~IWERK V_EQUI~TIDNR
*    INTO CORRESPONDING FIELDS OF TABLE T_EQUZ
*    FROM V_EQUI
*    WHERE V_EQUI~IWERK IN S_IWERK
*      AND V_EQUI~DATBI  = '99991231'.
*
*  IF SY-SUBRC <> 0.
*    IF  ( S_IWERK-LOW IS NOT INITIAL )
*    AND ( S_IWERK-HIGH IS NOT INITIAL ).
*      CONCATENATE 'Centro ' S_IWERK-LOW 'á' S_IWERK-HIGH 'não encontrado.' INTO MSG SEPARATED BY SPACE.
*    ELSEIF ( S_IWERK-LOW IS NOT INITIAL ).
*      CONCATENATE 'Centro ' S_IWERK-LOW 'não encontrada.' INTO MSG SEPARATED BY SPACE.
*    ENDIF.
*
*    MESSAGE MSG TYPE 'I'.
*    RETURN_CODE = 4.
*    EXIT.
*  ENDIF.

*     deleta equipamentos duplicados
*  SORT T_EQUZ BY IWERK EQUNR ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM T_EQUZ COMPARING EQUNR.
*
**--- determine number of entries
*  DESCRIBE TABLE T_EQUZ LINES T_EQUZ_MAX.
*
*  SELECT EQUNR AEDAT EQTYP EQART OBJNR
*    INTO CORRESPONDING FIELDS OF TABLE T_EQUI
*    FROM EQUI
*     FOR ALL ENTRIES IN T_EQUZ
*   WHERE EQUNR =  T_EQUZ-EQUNR
*     AND EQART IN S_EQART.
*
*  SELECT *
*     FROM ZTPARAM
*     INTO TABLE IT_ZTPARAM
*     WHERE PARAM EQ 'TP_OBJ'
*     AND ABASTEC EQ 'X'.
*
*  LOOP AT IT_ZTPARAM INTO WA_ZTPARAM.
*    WA_ABASTEC-SIGN = 'I'.
*    WA_ABASTEC-OPTION = 'EQ'.
*    WA_ABASTEC-LOW = WA_ZTPARAM-ZVAL.
*    APPEND WA_ABASTEC TO IT_ABASTEC.
*  ENDLOOP.
*
*  " Deleta categoria de equipamento que não estão na talbela it_abastec.
*  DELETE T_EQUI WHERE EQTYP NOT IN IT_ABASTEC.
*  ENDIF.

*     Seleciona Sistema
*  SELECT *
*    FROM MPOS
*    INTO CORRESPONDING FIELDS OF TABLE T_MPOS
*    FOR ALL ENTRIES IN T_EQUI
*    WHERE EQUNR = T_EQUI-EQUNR
*    AND BAUTL IN S_BAUTL
*    AND AUART IN S_TIPOO.

*  SELECT *
* FROM MPOS
* INNER JOIN IFLO AS IFLO ON IFLO~ILOAN EQ MPOS~ILOAN
* INTO CORRESPONDING FIELDS OF TABLE T_MPOS
**    FOR ALL ENTRIES IN T_EQUI
* WHERE BAUTL IN S_BAUTL
*   AND AUART IN S_TIPOO
*   AND INACT NE ABAP_TRUE.


*  LOOP AT T_MPOS INTO WA_MPOS.
*    IF WA_MPOS-WARPL IS NOT INITIAL AND WA_MPOS-WSTRA IS NOT INITIAL.
*      DELETE T_MPOS INDEX SY-TABIX.
*    ENDIF.
*  ENDLOOP.

  if p_dt is not initial.

*    IF IT_CALC-ZEIEH = 'WCH'."MON - MES "TAG - DIA    "WCH - SEMANA."AOENNING
    select *
        from mpla as a
          inner join mpos as b on a~warpl  =  b~warpl
          inner join mmpt as m on a~warpl  =  m~warpl
          inner join iloa as c on c~iloan  =  b~iloan
          inner join mhis as d on a~warpl  =  d~warpl
      into corresponding fields of table it_calc
      for all entries in t_mpos
        where a~warpl eq t_mpos-warpl and
*              A~MPTYP EQ 'NO'AND
              d~nplda > sy-datum and
              d~abnum eq ( select min( abnum ) from mhis where warpl eq d~warpl and nplda > sy-datum )
       and b~equnr in s_equnr
       and c~tplnr in s_tplnr
       and a~mptyp in s_mptyp
    and c~kostl in s_kostl.
    sort it_calc by nplda nplda.
  endif.

*  ENDIF.

  describe table t_mpos lines t_mpos_max.
  if t_mpos_max = 0.
    message 'Nenhum equipamento foi selecionado, com o criterio informado.' type 'I'.
    return_code = 4.
    exit.
  endif.


  " Tipo de equipamento
  if t_equi is not initial.
    select eqart eartx
      into table t_t370k_t
      from t370k_t
      for all entries in t_equi
      where eqart  =  t_equi-eqart
    and   spras  =  cc_spras .

    loop at t_equi.
      t_equi_tabix = sy-tabix .
      loop at t_equz where equnr = t_equi-equnr.
        read table t_t370k_t with key eqart = t_equi-eqart.
        t_equi-eartx =  t_t370k_t-eartx. " Tipo do objeto técnico
        t_equi-datbi =  t_equz-datbi.
        t_equi-iwerk =  t_equz-iwerk.
        t_equi-tidnr =  t_equz-tidnr.
        modify t_equi index t_equi_tabix.
      endloop.
    endloop.

*   Busca os status
    " I0076 - MREL --> marcado p/eliminação
    " I0320 - INAT --> oBJETO INATIVO
    select * from jest
      into table t_jest
      for all entries in t_equi
      where jest~objnr  eq t_equi-objnr
       and  inact       ne cc_x
    and  jest~stat   in ('I0076','I0320').

    describe table t_jest lines t_jest_max.
    if t_jest_max ne 0.
      perform exclui_inativo.

      describe table t_equi lines t_equi_max.
      if t_equi_max = 0.
        message 'Nenhum equipamento foi selecionado, com o criterio informado.' type 'I'.
        return_code = 4.
        exit.
      endif.
    endif.
  endif.

  data(aber) = 'I0001'.
  data(lib) =  'I0002'.

  "Verifica status da ordem.
  select *
  from afih as a
    inner join aufk as b on b~aufnr eq a~aufnr
  into corresponding fields of table t_afih
    for all entries in t_mpos
    where warpl eq t_mpos-warpl
    and ( exists ( select * from jest
                   where objnr eq b~objnr
                     and inact ne abap_true
                     and stat  in ( aber, lib ) ) ).

  sort t_afih descending by warpl equnr.

  perform prepara_relatorio.

***  Begin of    #101527  FF  01.03.2023
  if t_relatorio[] is initial.
    message 'Nenhum equipamento foi selecionado, com o criterio informado.' type 'I'.
    return_code = 4.
    exit.
  endif.
**  End of    #101527

endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PREPARA_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form prepara_relatorio.

  data: v_ktex1             type t351x-ktex1,
        lv_cycle_val        type f,
        lv_tabix_index_mmpt type sy-tabix,
        lv_tabix_mpos       type sy-tabix.

  clear: w_relatorio .
  free: t_relatorio.

**  Begin of    #101527  FF  01.03.2023
  if t_mpos[] is not initial.
    select equnr, eqktx
      from eqkt into table @data(lt_eqkt)
      for all entries in @t_mpos
      where equnr = @t_mpos-equnr
      and   spras = @sy-langu.

    select equnr, objnr
    from equi
    into table @data(lt_equi)
    for all entries in @t_mpos
    where equnr = @t_mpos-equnr.

  endif.
**  End of    #101527
*  LOOP AT T_EQUI.


  data: lv_line type bsvx-sttxt.

  loop at t_mpos.
    clear: w_relatorio.
*    LOOP AT T_EQUZ WHERE EQUNR = T_EQUI-EQUNR.


    "FF - inicio 21/11/23

    read table lt_equi with key equnr = t_mpos-equnr into data(wa_equi).
    if sy-subrc = 0.

      call function 'STATUS_TEXT_EDIT'
        exporting
          objnr            = wa_equi-objnr
          only_active      = 'X'
          spras            = sy-langu
        importing
          line             = lv_line
        exceptions
          object_not_found = 1
          others           = 2.
      if sy-subrc = 0.

        search lv_line for 'INAT'. "inativo
        if sy-subrc = 0 and p_equi is initial.
          continue.
        else.

          search lv_line for 'MREL'. "marcado para eliminar
          if sy-subrc = 0 and p_equi is initial.
            continue.
          endif.
        endif.

      endif.
    endif.

    "FF - fim


    w_relatorio-iwerk  = t_mpos-iwerk.
    w_relatorio-pltxt  = t_mpos-pltxt.
    w_relatorio-equnr  = t_mpos-equnr.
    w_relatorio-warpl  = t_mpos-warpl.
    w_relatorio-wptxt  = t_mpos-pstxt.
    w_relatorio-bautl  = t_mpos-bautl.
    w_relatorio-ordem  = icon_order.

*    CALL METHOD zcl_lis_ordem=>selec_ordem
*      EXPORTING
*        warpl = w_relatorio-warpl
*        equnr = w_relatorio-equnr
*      IMPORTING
*        aufnr = w_relatorio-w_ordem.


    if t_mpos-equnr is not initial.

      read table t_eqkt with key equnr = t_mpos-equnr.
      if sy-subrc = 0.
        w_relatorio-eqktx = t_eqkt-eqktx.
      endif.

      read table t_equi with key equnr = t_mpos-equnr.
      if sy-subrc = 0.

        w_relatorio-modelo = t_equi-typbz. "#101527  FF  01.03.2023

        read table t_t370k_t with key eqart = t_equi-eqart.
        if sy-subrc = 0.
          w_relatorio-eartx   = t_equi-eartx.
        endif.

      else.
        clear w_relatorio-modelo.
      endif.

      read table t_afih into data(w_afih) with key warpl = t_mpos-warpl equnr = t_mpos-equnr.
      if sy-subrc eq 0.
        w_relatorio-w_ordem = |{ w_afih-aufnr alpha = out }|.
      endif.

    else.
      read table t_afih into w_afih with key warpl = t_mpos-warpl.
      if sy-subrc eq 0.
        w_relatorio-w_ordem = |{ w_afih-aufnr alpha = out }|.
      endif.
    endif.
    clear: w_afih.


**  Begin of    #101527  FF  01.03.2023
    read table lt_eqkt assigning field-symbol(<fs_eqkt>) with key equnr = w_relatorio-equnr.
    if sy-subrc = 0.
      w_relatorio-shtxt = <fs_eqkt>-eqktx.
    else.
      clear w_relatorio-shtxt.
    endif.
**  End of    #101527

*    LOOP AT T_MPOS WHERE EQUNR = T_EQUI-EQUNR.

    if s_bautl-low is not initial and
       t_mpos-bautl <> s_bautl-low.
      continue.
    endif.

    select single * from mpla
    where warpl = t_mpos-warpl.

    perform status_check_plano using mpla-objnr return_code.
    if return_code <> 0 and  " ativos
       return_code <> 3   .  " inativo MREL
      continue.
    endif.

    select * from mmpt
      where warpl eq t_mpos-warpl.
    endselect.
    if sy-subrc eq 0.

      w_relatorio-zeieh = mmpt-zeieh.
      if p_ho is not initial.
        check w_relatorio-zeieh in t_zeieh.
      else .
        check w_relatorio-zeieh not in t_zeieh.
      endif.

      perform obtem_pos_tot_contador using mmpt-point.
      if t_mpos-wstra is initial.
        perform fltp_char_conversion_pak_f40 using fltp_char mmpt-zykl1 mmpt-zeieh.
        w_relatorio-zykl1    =  fltp_char+14(8).
        condense w_relatorio-zykl1.
        w_relatorio-pak_text = mmpt-pak_text.

        perform obtem_contadores using t_mpos-warpl mmpt-zeieh.

      else.

      endif.
    else.
      w_relatorio-zykl1      = ' '.
      w_relatorio-pak_text   = ' '.
    endif.
*    ENDLOOP.
*    ENDLOOP.
    clear: t_mpos, w_afih, w_relatorio-w_ordem.
  endloop.

endform.                    " PREPARA_RELATORIO

*&---------------------------------------------------------------------*
*&      Form  obtem_contadores
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WARPL    text
*----------------------------------------------------------------------*
form  obtem_contadores using p_warpl p_zeieh.
  clear: t_vimhis, w_relatorio-idat2.

  select * from vimhis
    into  table t_vimhis
  where warpl = p_warpl.

  if sy-subrc ne 0.
    return.
  endif.

*25.10.2023 - FF - inicio
  data(t_vimhis_aux) = t_vimhis[].

  delete t_vimhis_aux where tsabr is initial.
  delete t_vimhis_aux where lrmdt = 0.
  sort t_vimhis_aux by lrmdt descending.
*25.10.2023 - FF - fim

  loop at t_vimhis.

*25.10.2023 - FF - inicio
*    IF t_vimhis-tsabr NE space AND t_vimhis-lrmdt NE 0.  " Concluido
*      PERFORM fltp_char_conversion_pak_f40 USING fltp_char t_vimhis-rzaeh p_zeieh.
*
*      w_relatorio-rzaeh = fltp_char+14(8).           " Posição da última execução.
*      CONDENSE w_relatorio-rzaeh.
*      w_relatorio-idat2 = t_vimhis-lrmdt.            " Data de Conclusão
*
*      CONTINUE.
*
*    ENDIF.

    if t_vimhis_aux[] is not initial.
      data(wa_vimhis_aux) = t_vimhis_aux[ 1 ].

      perform fltp_char_conversion_pak_f40 using fltp_char wa_vimhis_aux-rzaeh p_zeieh.

*      w_relatorio-rzaeh = fltp_char+14(8).           " Posição da última execução.
*      CONDENSE w_relatorio-rzaeh.

      v_reinicio1 = fltp_char+14(8).           " Posição da última execução.
      condense v_reinicio1.

      w_relatorio-idat2 = wa_vimhis_aux-lrmdt.       " Data de Conclusão

    else.

      if t_vimhis-tsabr ne space.                      " Solicitado.
        perform fltp_char_conversion_pak_f40 using fltp_char t_vimhis-szaeh p_zeieh.
*        CONDENSE  w_relatorio-szaeh.
        v_reinicio2 =  fltp_char+14(8).         " Reinicio do plano
        condense v_reinicio2.

      endif.

    endif.

    if v_reinicio1 > v_reinicio2.
      w_relatorio-rzaeh = v_reinicio1.
    else.
      w_relatorio-rzaeh = v_reinicio2.
    endif.

    clear: v_reinicio1, v_reinicio2.
*25.10.2023 - FF - fim.

    select single *
      from mhio
      where warpl eq t_vimhis-warpl
    and  abnum eq t_vimhis-abnum.

    if t_vimhis-tsabr ne space and mhio-addat ne 0.  " Concluido
      continue.
    endif.

    if t_vimhis-tstat = cc_x.                        " Ignorado.
      continue.
    endif.

    if t_vimhis-tsabr ne space.                      " Solicitado.
      perform fltp_char_conversion_pak_f40 using fltp_char t_vimhis-szaeh p_zeieh.
      w_relatorio-szaeh  =  fltp_char+14(8).         " Posição do contador.
      condense  w_relatorio-szaeh.

      perform fltp_char_conversion_pak_f40 using fltp_char t_vimhis-nzaeh p_zeieh.
      w_relatorio-nzaeh  =  fltp_char+14(8).       " Proxima leitura programa
      condense w_relatorio-nzaeh.

      if t_mpos-wstra is initial.
*        gv_uso    = wa_imrg-readg - t_vimhis-nzaeh + mmpt-zykl1. "FF 26.10.2023
        gv_uso    = wa_imrg-readg - wa_vimhis_aux-nzaeh + mmpt-zykl1. "FF 26.10.2023

        gv_desvio = gv_uso - mmpt-zykl1.
      else.
*        gv_uso    = wa_imrg-readg - t_vimhis-nzaeh + mmpt_tab-zykl1. "FF 26.10.2023
        gv_uso    = wa_imrg-readg - wa_vimhis_aux-nzaeh + mmpt_tab-zykl1. "FF 26.10.2023

        gv_desvio = gv_uso - mmpt_tab-zykl1.
      endif.

      perform fltp_char_conversion_pak_f40 using fltp_char gv_uso p_zeieh.

**  Begin of  #101527  FF  01.03.2023
*            f-uso    =  fltp_char+14(8).       " Uso

      w_relatorio-uso    =  w_relatorio-totac - w_relatorio-rzaeh.    " Uso

      condense w_relatorio-uso.

*      PERFORM fltp_char_conversion_pak_f40 USING fltp_char gv_desvio p_zeieh.
*      w_relatorio-desvio =  fltp_char+14(8).       " Desvio

      "w_relatorio-desvio = uso - ciclo
      w_relatorio-desvio = w_relatorio-uso - w_relatorio-zykl1.
      condense w_relatorio-desvio.

      if w_relatorio-desvio <= 0.
        w_relatorio-avenc = 'X'.
      else.
        w_relatorio-venc = 'X'.
      endif.

** End of FF  01.03.2023


      if w_relatorio-zykl1 > 0.
        w_relatorio-per_desv = ( w_relatorio-desvio / w_relatorio-zykl1 ) * 100.
      else.
        w_relatorio-per_desv = 0.
      endif.

      perform agrupa_calc using p_warpl w_relatorio-idat2.
      perform calc using p_dt p_warpl.

      if p_dt is not initial.
*          Mudando a cor dos ponto em atraso
        if w_relatorio-desvio > 0.
          clear wa_color.
          move 'DESVIO'   to wa_color-fname.
          move '6'        to wa_color-color-col.
          move '1'        to wa_color-color-int.
          move '1'        to wa_color-color-inv.
          append wa_color to it_color.
          w_relatorio-cell_color[] = it_color[].
*          w_relatorio-line_color = 'C610'.
        endif.

        perform f_uso_color. "Colorindo a celula USO -- #101527  FF  01.03.2023

        if w_relatorio-szaeh_ <> w_relatorio-rzaeh.
          clear wa_color.
          move 'SZAEH_'    to wa_color-fname.
          move '3'        to wa_color-color-col.
          move '1'        to wa_color-color-int.
          move '1'        to wa_color-color-inv.
          append wa_color to it_color.

          clear wa_color.
          move 'RZAEH'    to wa_color-fname.
          move '3'        to wa_color-color-col.
          move '1'        to wa_color-color-int.
          move '1'        to wa_color-color-inv.
          append wa_color to it_color.
*        w_relatorio-line_color = 'C610'.
        endif.

      else.
*         Mudando a cor dos ponto em atraso
        if w_relatorio-desvio > 0.
          clear wa_color.
          move 'DESVIO'   to wa_color-fname.
          move '6'        to wa_color-color-col.
          move '1'        to wa_color-color-int.
          move '1'        to wa_color-color-inv.
          append wa_color to it_color.
          w_relatorio-cell_color[] = it_color[].
*          w_relatorio-line_color = 'C610'.
*          w_relatorio-venc = 'X'.
        endif.

        perform f_uso_color. "Colorindo a celula USO -- #101527  FF  01.03.2023

        if w_relatorio-szaeh <> w_relatorio-rzaeh.
          clear wa_color.
          move 'SZAEH'    to wa_color-fname.
          move '3'        to wa_color-color-col.
          move '1'        to wa_color-color-int.
          move '1'        to wa_color-color-inv.
          append wa_color to it_color.

          clear wa_color.
          move 'RZAEH'    to wa_color-fname.
          move '3'        to wa_color-color-col.
          move '1'        to wa_color-color-int.
          move '1'        to wa_color-color-inv.
          append wa_color to it_color.
*        w_relatorio-line_color = 'C610'.
        endif.

        w_relatorio-cell_color[] = it_color[].
*

        append w_relatorio to t_relatorio.

        clear: w_relatorio-venc,
               w_relatorio-avenc,
               "W_RELATORIO-TIDNR,
               w_relatorio-equnr,
               w_relatorio-pltxt,
               w_relatorio-eqktx,
               w_relatorio-ordem,
               w_relatorio-shtxt,
               w_relatorio-eartx,
               w_relatorio-cell_color[],
*             W_RELATORIO-RZAEH,
               t_vimhis,
               w_relatorio-cell_color,
               it_color[].
        exit.
      endif.
    endif.

    if t_vimhis-tsvbt ne space and t_vimhis-tsabr eq space.
      if t_vimhis-tsenq ne space.                      "Espera.
        perform fltp_char_conversion_pak_f40 using fltp_char t_vimhis-szaeh p_zeieh.
        w_relatorio-szaeh  =  fltp_char+14(8).       " Posição do contador.
        condense w_relatorio-szaeh.


        perform fltp_char_conversion_pak_f40 using fltp_char t_vimhis-nzaeh p_zeieh.
        w_relatorio-nzaeh  =  fltp_char+14(8).       " Proxima leitura programa
        condense w_relatorio-nzaeh.

        if t_mpos-wstra is initial.
*          gv_uso    = wa_imrg-readg - t_vimhis-nzaeh + mmpt-zykl1. "FF 25.10.2023
          gv_uso    = wa_imrg-readg - wa_vimhis_aux-nzaeh + mmpt-zykl1. "FF 25.10.2023
          gv_desvio = gv_uso - mmpt-zykl1.
        else.
*          gv_uso    = wa_imrg-readg - t_vimhis-nzaeh + mmpt_tab-zykl1. "FF 25.10.2023
          gv_uso    = wa_imrg-readg - wa_vimhis_aux-nzaeh + mmpt_tab-zykl1. "FF 25.10.2023
          gv_desvio = gv_uso - mmpt_tab-zykl1.
        endif.

        perform fltp_char_conversion_pak_f40 using fltp_char gv_uso p_zeieh.
**  Begin of  #101527  FF  01.03.2023
*            w_relatorio-uso    =  fltp_char+14(8).       " Uso


        if w_relatorio-w_ordem is not initial.
          w_relatorio-uso    =  w_relatorio-totac - w_relatorio-rzaeh.    " Uso
        else.
          w_relatorio-uso    =  ( w_relatorio-totac - w_relatorio-rzaeh ) - w_relatorio-szaeh.    " Uso
        endif.

        condense w_relatorio-uso.

*        PERFORM fltp_char_conversion_pak_f40 USING fltp_char gv_desvio p_zeieh.
*        w_relatorio-desvio =  fltp_char+14(8).       " Desvio

        "w_relatorio-desvio = uso - ciclo
        w_relatorio-desvio = w_relatorio-uso - w_relatorio-zykl1.
        condense w_relatorio-desvio.


        if w_relatorio-desvio <= 0.
          w_relatorio-avenc = 'X'.
        else.
          w_relatorio-venc = 'X'.
        endif.

** End of FF  01.03.2023

        if w_relatorio-zykl1 > 0.
          w_relatorio-per_desv = ( w_relatorio-desvio / w_relatorio-zykl1 ) * 100.
        else.
          w_relatorio-per_desv = 0.
        endif.


        perform agrupa_calc using p_warpl w_relatorio-idat2.
        perform calc using p_dt p_warpl.



        if p_dt is not initial.
*          Mudando a cor dos ponto em atraso
          if w_relatorio-desvio > 0.
            clear wa_color.
            move 'DESVIO'   to wa_color-fname.
            move '6'        to wa_color-color-col.
            move '1'        to wa_color-color-int.
            move '1'        to wa_color-color-inv.
            append wa_color to it_color.
            w_relatorio-cell_color[] = it_color[].
*          w_relatorio-line_color = 'C610'.
*            w_relatorio-venc = 'X'.
          endif.

          perform f_uso_color. "Colorindo a celula USO -- #101527  FF  01.03.2023

          if w_relatorio-szaeh_ <> w_relatorio-rzaeh.
            clear wa_color.
            move 'SZAEH_'    to wa_color-fname.
            move '3'        to wa_color-color-col.
            move '1'        to wa_color-color-int.
            move '1'        to wa_color-color-inv.
            append wa_color to it_color.

            clear wa_color.
            move 'RZAEH'    to wa_color-fname.
            move '3'        to wa_color-color-col.
            move '1'        to wa_color-color-int.
            move '1'        to wa_color-color-inv.
            append wa_color to it_color.
*        w_relatorio-line_color = 'C610'.
          endif.

        else.
*         Mudando a cor dos ponto em atraso
          if w_relatorio-desvio > 0.
            clear wa_color.
            move 'DESVIO'   to wa_color-fname.
            move '6'        to wa_color-color-col.
            move '1'        to wa_color-color-int.
            move '1'        to wa_color-color-inv.
            append wa_color to it_color.
            w_relatorio-cell_color[] = it_color[].
*          w_relatorio-line_color = 'C610'.
*            w_relatorio-venc = 'X'.
          endif.

          perform f_uso_color. "Colorindo a celula USO -- #101527  FF  01.03.2023

          if w_relatorio-szaeh <> w_relatorio-rzaeh.
            clear wa_color.
            move 'SZAEH'    to wa_color-fname.
            move '3'        to wa_color-color-col.
            move '1'        to wa_color-color-int.
            move '1'        to wa_color-color-inv.
            append wa_color to it_color.

            clear wa_color.
            move 'RZAEH'    to wa_color-fname.
            move '3'        to wa_color-color-col.
            move '1'        to wa_color-color-int.
            move '1'        to wa_color-color-inv.
            append wa_color to it_color.
*        w_relatorio-line_color = 'C610'.
          endif.
        endif.

        w_relatorio-cell_color[] = it_color[].
*
        append w_relatorio to t_relatorio.
        clear: w_relatorio-venc,
               w_relatorio-avenc,
                "W_RELATORIO-TIDNR,
               w_relatorio-equnr,
               w_relatorio-pltxt,
               w_relatorio-eqktx,
               w_relatorio-ordem,
               w_relatorio-shtxt,
               w_relatorio-eartx,
               w_relatorio-cell_color[],
*               W_RELATORIO-RZAEH,
               t_vimhis,
               w_relatorio-cell_color,
               it_color[].

      endif.
    endif.
  endloop.

  delete adjacent duplicates from t_relatorio comparing iwerk eartx warpl zykl1(8) pak_text wptxt szaeh. "(8) TOTAC(8) PLTXT EQKTX
*                                                         NZAEH(8) USO(8) DESVIO(8) PER_DESV IDAT2 BAUTL ICON.

**  Begin of    #101527  FF  01.03.2023
  if p_venc is not initial and
     p_avenc is not initial.

    delete t_relatorio where avenc is initial and venc is initial.

  else.

    if p_venc = 'X'.
*      DELETE t_relatorio WHERE desvio <= 0.
      delete t_relatorio where not venc = 'X'.
    endif.

    if p_avenc = 'X'.
*      DELETE t_relatorio WHERE desvio > 0.
      delete t_relatorio where not avenc = 'X'.
    endif.
  endif.

  loop at t_relatorio assigning field-symbol(<fs>) where equnr is initial.
    clear <fs>-shtxt.
  endloop.
** End of FF  01.03.2023

endform.                    "obtem_contadores

*&---------------------------------------------------------------------*
*&      Form  fltp_char_conversion_pak_f40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CHAR_WERT  text
*      -->FLTP_WERT  text
*      -->EINHEIT    text
*----------------------------------------------------------------------*
form fltp_char_conversion_pak_f40 using char_wert
                                        fltp_wert
                                        einheit.
  clear char_wert.
  check not einheit is initial.

  call function 'FLTP_CHAR_CONVERSION_FROM_SI'
    exporting
      char_unit       = einheit
      decimals        = 0
      exponent        = 0
      fltp_value_si   = fltp_wert
      indicator_value = cc_x
      masc_symbol     = ' '
    importing
      char_value      = char_wert.

endform.                    "fltp_char_conversion_pak_f40

*&---------------------------------------------------------------------*
*&      Form  exclui_inativo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form exclui_inativo.
  data: lv_tabix      type sy-tabix.
* delete duplicate indices
  sort t_jest by objnr.

  delete adjacent duplicates from t_jest comparing objnr.

  loop at t_jest.
    delete t_equi where objnr = t_jest-objnr.
  endloop.

endform.                    "exclui_inativo

*&---------------------------------------------------------------------*
*&      Form  Obtem_Pos_Tot_contador
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->POINT      text
*----------------------------------------------------------------------*
form obtem_pos_tot_contador using point.

  data : no_beleg like sy-subrc,
         fltp     type imrc_totac.

  call function 'MEASUREM_POINT_LAST_VALUE'
    exporting
      i_point           = point
    importing
      e_wa_point        = wa_impt
      e_point_txt       = point_txt
      e_wa_value        = wa_imrg
      e_no_value        = no_beleg
    exceptions
      pointer_not_found = 01.

  case sy-subrc.
    when 0.
      if no_beleg is initial.
        if not wa_imrg-readg is initial.
          perform fltp_char_conversion_pak_f40
             using fltp_char
                   wa_imrg-readg
                   wa_impt-msehi.
          w_relatorio-totac  =  fltp_char+14(8).
        endif.
      endif.
    when others.
      clear: w_relatorio-totac.
  endcase.

endform.                    "Obtem_Pos_Tot_contador


*&---------------------------------------------------------------------*
*&      Form  obtem_ciclos_strat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STRAT    text
*      -->P_WARPL    text
*----------------------------------------------------------------------*
form obtem_ciclos_strat using p_strat p_warpl.

  call function 'IWP1_STRATEGY_READ'
    exporting
      i_strat                   = p_strat
      i_multicounterplan        = ' '
    tables
      t_package_data            = g_t_package_data
    changing
      c_t351                    = g_w_t351
    exceptions
      strategy_is_initial       = 1
      strategy_not_found        = 2
      strategy_without_packages = 3
      no_cycle_set              = 4
      no_strategy               = 5
      others                    = 6.
  if sy-subrc ne 0.
    exit.
  endif.

  clear    g_t_package_data_st_tl.
  refresh  g_t_package_data_st_tl.
  clear impos.    refresh impos.
  move-corresponding t_mpos to impos.
  move t_mpos-wppos         to impos-posnr.
  move cc_x                 to impos-dbknz.
  append impos.
  sort impos by posnr.

*--- determine the used packages in plan/item for
*    strategy releated scheduling
  check not p_strat is initial.

  if impos-plnal ne space and
     impos-plnnr ne space.
*--- update table with package data
    perform update_package_data_st_tl using impos
                                             ' '  " t399w-call_type
                                             ' '
                                             '19000101'.
  endif.

*--- sort table
  sort g_t_package_data_st_tl ascending by zaehlstrat
                                           datuvplwp.

*--- determine the number of used packages
  describe table g_t_package_data_st_tl lines anz_verpak.


endform.                    "obtem_ciclos_strat

*&---------------------------------------------------------------------*
*&      Form  update_package_data_st_tl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITEM         text
*      -->P_CALL_OBJECT  text
*      -->P_ACTIVITY     text
*      -->P_DATE_BEGIN   text
*----------------------------------------------------------------------*
form update_package_data_st_tl using p_item        type ty_wmpos
                                     p_call_object like t399w-call_type
                                     p_activity    type c
                                     p_date_begin  like sy-datum.

*--- information
*    p_activity = ' ' -> update
*    p_activity = 1   -> delete all entries in package table
*                        related to the item
*--- data definition
  data: l_t_plwp like plwp occurs 0 with header line.

*--- build the package data table only in case of maintenance plan
*    processing!
  if not p_activity = 1.
*--- in case of no deletion only
*--- read all used package in task list
    call function 'CI03_READ_PLWP_TIME_INTERVAL'
      exporting
        i_plnty      = p_item-plnty
        i_plnnr      = p_item-plnnr
        i_plnal      = p_item-plnal
        i_date_begin = p_date_begin
        i_date_end   = '99991231'
      importing
        e_plwp_tab   = l_t_plwp[].
  endif.

*--- build internal table with package information
  call function 'Z_PACKAGES_STRATEGY_TL'
    exporting
      i_activity               = p_activity
      i_item                   = p_item
      i_call_object            = p_call_object
    tables
      t_package_data_st_tl     = g_t_package_data_st_tl
      t_package_data_task_list = l_t_plwp
      t_package_data_strategy  = g_t_package_data.
  perform wstra_mmpt_f10.

endform.                    "update_package_data_st_tl

*&---------------------------------------------------------------------*
*&      Form  wstra_mmpt_f10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form wstra_mmpt_f10.

*  CLEAR anz_mmptnr.
  clear mmpt_tab.
  refresh mmpt_tab.


  loop at g_t_package_data.
*....... MMPT_TAB füllen
    clear mmpt_tab.
    move: t_mpos-warpl                to mmpt_tab-warpl,
          g_t_package_data-pakett351x to mmpt_tab-nummer,
          g_t_package_data-zeieht351p to mmpt_tab-zeieh,
          g_t_package_data-zykztt351p to mmpt_tab-zykl1,
          g_t_package_data-offztt351p to mmpt_tab-offset,
          g_t_package_data-ktex1t351x to mmpt_tab-pak_text,
          g_t_package_data-koff1t351x to mmpt_tab-koff1,
          g_t_package_data-kzyk1t351x to mmpt_tab-kzyk1.

    append mmpt_tab.

  endloop.
endform.                               " WSTRA_MMPT_F10

*&---------------------------------------------------------------------*
*&      Form  index_mmpt_used_packages
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WPPOS    text
*----------------------------------------------------------------------*
form index_mmpt_used_packages using p_wppos like mpos-wppos.

*--- data definition
  data:   save_tabix like sy-tabix.
  ranges: r_wppos    for g_t_package_data_st_tl-wppos.


*--- only item should be checked in case of filled p_wppos
  if not p_wppos is initial.
    r_wppos-sign   = 'I'.
    r_wppos-option = 'EQ'.
    r_wppos-low    = p_wppos.
    append r_wppos.
  endif.
  sttag = sy-datum.
  call function 'CI03_READ_PLKO'
    exporting
      i_plnty         = t_mpos-plnty
      i_plnnr         = t_mpos-plnnr
      i_plnal         = t_mpos-plnal
      i_date          = sttag
    exceptions
      tl_not_existent = 1
      tl_not_valid    = 2.

  case sy-subrc.
    when 1.
      concatenate 'A lista de tarefa' t_mpos-plnty t_mpos-plnnr t_mpos-plnal 'não existe.' into msg.
      message msg type 'E'.
      exit.
    when 2.
      concatenate 'Não existe lista de tarefas'  t_mpos-plnty t_mpos-plnnr t_mpos-plnal 'na data fixada' into msg.
      message msg type 'E'.
      exit.
  endcase.

*--- init data
  clear save_tabix.
  clear index_mmpt.
  refresh index_mmpt.

  select single * from  t399w
  where  mptyp       = t_mpos-mityp.

* get valid packages
  call function 'Z_PACKAGES_SET_IND_VALIDITY'
    exporting
      i_date               = sttag
      i_call_object        = t399w-call_type
      i_item_row_no        = p_wppos
    tables
      t_package_data_st_tl = g_t_package_data_st_tl.

*--- create index for mmpt_tab
  loop at mmpt_tab
    where upd_knz ne wc_dele.
    save_tabix = sy-tabix.
    loop at g_t_package_data_st_tl
      where wppos      in r_wppos           and
            zaehlstrat =  mmpt_tab-nummer.
      if not g_t_package_data_st_tl-ind_valid is initial.
        move save_tabix to index_mmpt-tabpt.
        append index_mmpt.
      endif.
    endloop.
  endloop.

* delete duplicate indices
  sort index_mmpt by tabpt.
  delete adjacent duplicates from index_mmpt comparing tabpt.

*--- determine number of entries
  describe table index_mmpt lines mmpt_max.
endform.                    "index_mmpt_used_packages


*&---------------------------------------------------------------------*
*&      Form  STATUS_CHECK_PLANO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->MPLA_OBJNR   text
*      -->RETURN_CODE  text
*----------------------------------------------------------------------*
form status_check_plano using mpla_objnr return_code.

  constants: y_i0013_lokz like jest-stat  value 'I0013',
             y_i0076_lovm like jest-stat  value 'I0076',
             y_i0320_inak like jest-stat  value 'I0320'.

  data: begin of ijstat occurs   0.
          include structure jstat.
  data: end of ijstat.

  call function 'STATUS_READ'
    exporting
      client      = sy-mandt
      objnr       = mpla_objnr
      only_active = 'X'
    tables
      status      = ijstat.

  loop at ijstat.

    case ijstat-stat.
      when y_i0013_lokz.
        return_code = 1.
      when y_i0076_lovm.
        return_code = 2.
        exit.
      when y_i0320_inak.               "Inativo
        return_code = 3.
      when others.
        return_code = 0.
    endcase.

  endloop.
endform.                    "STATUS_CHECK_PLANO
*&---------------------------------------------------------------------*
*&      Form  CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DT  text
*      -->P_P_WARPL  text
*----------------------------------------------------------------------*
form calc  using    v_dt
                    v_warpl.


  if v_dt is not initial.
    try .
        data(wa) = it_calc[ warpl = v_warpl ].
      catch cx_sy_itab_line_not_found.
        clear wa.
    endtry.

    w_relatorio-uso      = wa-uso.
    w_relatorio-desvio   = wa-desvio.
    w_relatorio-per_desv = wa-per_desv.
*    W_RELATORIO-SZAEH_    = |{ WA-STADT+6(2) }{ WA-STADT+4(2) }{ WA-STADT(4) }|.
*    W_RELATORIO-NZAEH_    = |{ WA-NPLDA+6(2) }{ WA-NPLDA+4(2) }{ WA-NPLDA(4) }|.
    w_relatorio-szaeh_    = wa-stadt.
    w_relatorio-nzaeh_    = wa-nplda.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  AGRUPA_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form agrupa_calc using v_warpl v_idat2.

  if p_dt is not initial.

    data: convert      type i,
          data_proxima type mhis-lrmdt.

    loop at it_calc assigning field-symbol(<calc>) where warpl eq v_warpl.

      <calc>-lrmdt = v_idat2.

      perform fltp_char_conversion_pak_f40
          using fltp_char
                <calc>-zykl1
                <calc>-zeieh.

      condense fltp_char no-gaps.

      <calc>-zykl2 = fltp_char. "CICLO

      if <calc>-stadt is not initial and <calc>-lrmdt is not initial.
        data(dt_menor1) = sy-datum - <calc>-stadt.
        data(dt_menor2) = sy-datum - <calc>-lrmdt.
        if dt_menor1 < dt_menor2.
          data_proxima = <calc>-stadt.
        else.
          data_proxima = <calc>-lrmdt.
        endif.
      elseif <calc>-stadt is not initial and <calc>-lrmdt is initial.
        data_proxima = <calc>-stadt.
      elseif <calc>-stadt is initial and <calc>-lrmdt is not initial.
        data_proxima = <calc>-lrmdt.
      endif.

      case <calc>-zeieh.
        when 'WCH'. "Semanas
          convert =  ( ( sy-datum - data_proxima ) / 7 ).  "CALC USO SEMANAS
        when 'MON'. "Meses
          convert =  ( ( sy-datum - data_proxima ) / 30 ). "CALC USO MESES
        when 'TAG'. "Dias
          convert =  ( sy-datum - data_proxima ).          "CALC USO DIAS
      endcase.

      <calc>-uso =  convert. "CONVERT USO

      convert =  <calc>-uso - <calc>-zykl2. " CALC DESVIO
      <calc>-desvio =  convert. " CONVERT DESVIO

      convert = convert * -1.
      <calc>-per_desv =  ( ( convert / <calc>-zykl2 ) * 100 ). " PORCENTAGEM DO DESVIO

    endloop.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  SEL_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
form sel_ordem  using p_row p_column_fieldname.

  free clicks.
  add 1 to clicks.

  free t_planos.
  move-corresponding t_relatorio to t_planos.
  try .
      data(wa_planos) = t_planos[ p_row ].
    catch cx_sy_itab_line_not_found.
  endtry.

  case p_column_fieldname.

    when 'ORDEM'.
      wa_planos-warpl = |{ wa_planos-warpl alpha = in }|.

      select *
      from viaufkst
      into corresponding fields of table t_ordem
      where warpl eq wa_planos-warpl.
      sort t_ordem ascending by erdat.

      select *
      from viqmel
      into corresponding fields of table t_viqmel
      where iwerk eq wa_planos-iwerk
      and warpl eq wa_planos-warpl.
      sort t_viqmel ascending by qmdat.

      if not t_ordem is initial.
        free linha_selecionada.
        free _exit.
        loop at t_ordem assigning field-symbol(<w_ordem>).

          if <w_ordem>-objnr is not initial.
            call function 'STATUS_TEXT_EDIT'
              exporting
                flg_user_stat    = yx
                objnr            = <w_ordem>-objnr             "1695763
                spras            = sy-langu
              importing
                line             = <w_ordem>-sttxt             "1695763
                user_line        = <w_ordem>-asttx             "1695763
              exceptions
                object_not_found = 1
                others           = 2.

            if sy-subrc = 0.
              <w_ordem>-status = <w_ordem>-sttxt.
              <w_ordem>-status = <w_ordem>-status(4).
            endif.
          endif.
        endloop.

        if ( t_ordem is not initial ).
*      DELETE T_ORDEM WHERE WARPL NE WA_PLANOS-WARPL.

          data(tl_fieldcat) = value slis_t_fieldcat_alv(

          ( fieldname = 'BUKRS     '        seltext_m = 'Empresa '  outputlen = '07' )
          ( fieldname = 'WERKS     '        seltext_m = 'Centro  '  outputlen = '04' )
          ( fieldname = 'WARPL     '        seltext_m = 'Plano   '  outputlen = '10' )
          ( fieldname = 'AUFNR     '        seltext_m = 'Ordem   '  outputlen = '10' )
          ( fieldname = 'STATUS    '        seltext_m = 'Status  '  outputlen = '10' )
          ( fieldname = 'ERDAT     '        seltext_m = 'Data    '  outputlen = '10' ) ).

          call function 'REUSE_ALV_POPUP_TO_SELECT'
            exporting
              i_title     = 'Selecionar ordem de plano'
              i_selection = 'X'
              i_tabname   = 'T_ORDEM'
              i_zebra     = 'X'
              it_fieldcat = tl_fieldcat
            importing
              es_selfield = linha_selecionada
              e_exit      = _exit
            tables
              t_outtab    = t_ordem.

        else.
          free linha_selecionada.
          free _exit.
          check ( t_viqmel is not initial ).
          loop at t_viqmel assigning field-symbol(<w_viqmel>).
            if <w_viqmel>-objnr is not initial.
              call function 'STATUS_TEXT_EDIT'
                exporting
                  flg_user_stat    = yx
                  objnr            = <w_viqmel>-objnr             "1695763
                  spras            = sy-langu
                importing
                  line             = <w_viqmel>-sttxt             "1695763
                  user_line        = <w_viqmel>-asttx             "1695763
                exceptions
                  object_not_found = 1
                  others           = 2.

              if sy-subrc = 0.
                <w_viqmel>-status = <w_viqmel>-sttxt.
                <w_viqmel>-status = <w_viqmel>-status(4).
              endif.
            endif.
          endloop.

          data(_tl_fieldcat) = value slis_t_fieldcat_alv(

          ( fieldname = 'BUKRS     '        seltext_m = 'Empresa       '  outputlen = '07' )
          ( fieldname = 'IWERK     '        seltext_m = 'Centro        '  outputlen = '04' )
          ( fieldname = 'WARPL     '        seltext_m = 'Plano         '  outputlen = '10' )
          ( fieldname = 'QMNUM     '        seltext_m = 'Nota          '  outputlen = '10' )
          ( fieldname = 'STATUS    '        seltext_m = 'Status        '  outputlen = '10' )
          ( fieldname = 'QMDAT     '        seltext_m = 'Data da nota  '  outputlen = '10' ) ).

          call function 'REUSE_ALV_POPUP_TO_SELECT'
            exporting
              i_title     = 'Selecionar notas de plano'
              i_selection = 'X'
              i_tabname   = 'T_VIQMEL'
              i_zebra     = 'X'
              it_fieldcat = _tl_fieldcat
            importing
              es_selfield = linha_selecionada
              e_exit      = _exit
            tables
              t_outtab    = t_viqmel.

        endif.
      else.
        message 'Não existe ordem para o plano selecionado' type 'I' display like 'I'.
        exit.
      endif.

      case linha_selecionada-fieldname.
        when 'AUFNR' or 'W_ORDEM'.
          set parameter id 'ANR' field linha_selecionada-value.
          call transaction 'IW32' and skip first screen .
        when 'WARPL'.
          set parameter id 'MPL' field linha_selecionada-value.
          call transaction 'IP10' and skip first screen.
        when 'QMNUM'.
          set parameter id 'IQM' field linha_selecionada-value.
          call transaction 'IW22' and skip first screen.
        when others.
      endcase.

    when 'WARPL'.
      wa_planos-warpl = |{ wa_planos-warpl alpha = in }|.
      set parameter id 'MPL' field wa_planos-warpl.
      call transaction 'IP10' and skip first screen.

    when 'EQUNR'.
      set parameter id 'EQN' field wa_planos-equnr.
      call transaction 'IE03' and skip first screen.

    when 'W_ORDEM'.
      set parameter id 'ANR' field wa_planos-w_ordem.
      call transaction 'IW32' and skip first screen .


  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  IT_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1335   text
*      -->P_1336   text
*      -->P_1337   text
*      -->P_1338   text
*      -->P_1339   text
*      -->P_1340   text
*      -->P_1341   text
*      -->P_1342   text
*----------------------------------------------------------------------*
form it_catalog  using value(p_colnum)
                  value(p_fieldname)
                  value(p_tabname)
                  value(p_len)
                  value(p_edit)
                  value(p_icon)
                  value(p_do_sum)
                  value(p_header)
                  value(p_emphasize)
                  value(p_hotspot)
                  value(p_ref_table)
                  value(p_ref_field)
                  value(p_no_zero)
                  value(p_outputlen).



  data:  wa_fieldcatalog  type lvc_s_fcat.
  clear: wa_fieldcatalog,
  gs_layout.

  wa_fieldcatalog-col_pos     = p_colnum.
  wa_fieldcatalog-fieldname   = p_fieldname.
  wa_fieldcatalog-tabname     = p_tabname.
  wa_fieldcatalog-outputlen   = p_len.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-do_sum      = p_do_sum.
  wa_fieldcatalog-coltext     = p_header.
  wa_fieldcatalog-emphasize   = p_emphasize.
  wa_fieldcatalog-hotspot     = p_hotspot.
  wa_fieldcatalog-ref_table   = p_ref_table.
  wa_fieldcatalog-ref_table   = p_ref_field.
  wa_fieldcatalog-no_zero     = p_no_zero.
  wa_fieldcatalog-outputlen   = p_outputlen.

  gs_layout-info_fname     = 'ROWCOLOR'.  "Row color
  gs_layout-ctab_fname     = 'CELL_COLOR'.
  gs_layout-excp_conds     = 'X'.
  gs_layout-zebra          = 'X'.
  gs_layout-sel_mode       = 'A'.
  gs_layout-cwidth_opt     = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef     = ' '.

  append wa_fieldcatalog to it_s_fcat.




endform.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ENDMETHOD  text
*----------------------------------------------------------------------*
form f_bdc_data  using    p_program p_dynpro p_start p_fnam p_fval.

  append value #(
                program   = p_program
                dynpro    = p_dynpro
                dynbegin  = p_start
                fnam      = p_fnam
                fval      = p_fval
  ) to ti_bdcdata.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1709   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*


form zf_call_transaction using p_trans changing p_erro.

  constants: c_msgid like it_msg-msgid value 'F5',
             c_msgnr like it_msg-msgnr value '312',
             c_msgne like it_msg-msgnr value '539'.

  data: wl_cont    type sy-tabix,
        wl_mode(1).

*  FREE IT_MSG .
  clear wl_mode.
  clear wl_cont.
  wl_mode = 'E'.

  call transaction p_trans using ti_bdcdata
                           mode wl_mode
                           messages into it_msg.

  clear: wl_cont.

  if line_exists( it_msg[ msgtyp = 'A' ] ).
    p_erro = abap_true.
  else.
    if line_exists( it_msg[ msgtyp = 'E' ] ).
      p_erro = abap_true.
    endif.
  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_USO_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_uso_color .

  check w_relatorio-desvio <= 0.

  " lv_horizonte = Horizonte * Ciclo
  data(lv_horizonte) = ( t_vimhis-horiz / 100 ) * w_relatorio-zykl1.

  if w_relatorio-uso >= lv_horizonte.
    clear wa_color.
    move 'USO'      to wa_color-fname.
    move '3'        to wa_color-color-col. "Amarelo
    move '1'        to wa_color-color-int.
    move '1'        to wa_color-color-inv.
    append wa_color to it_color.
    w_relatorio-cell_color[] = it_color[].
*    w_relatorio-avenc = 'X'.
  endif.

endform.
