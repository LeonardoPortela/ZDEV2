*&---------------------------------------------------------------------*
*& Include ZSDR0040TOP                                       PoolMóds.        ZSDR0040
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*----------------------------------------------------------------------*
*&  Data      | Request    | Autor         | Alteração                 *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 07/05/2025 | DEVK9A2BIM | NSEGATIN      | Ajuste Melhorias Parte 2  *
*&                                         | Cadastro Limite de dias.  *
*----------------------------------------------------------------------*
PROGRAM  zsdr0040.

TABLES: vbak.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES:
  BEGIN OF ty_fields,
    campo(30) TYPE c,
    group1(5) TYPE c,
    value     TYPE sy-tabix,
    invisible TYPE sy-tabix,
  END   OF ty_fields.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: ok-code         TYPE sy-ucomm,
      wg_mensagem(30),
      wg_acao(30),

      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,

      BEGIN OF tg_itens OCCURS 0,
        matkl   TYPE zsdt0087-matkl,
        tpsim   TYPE zsdt0087-tpsim,
        auart   TYPE zsdt0087-auart,
        inco1   TYPE zsdt0087-inco1,
        spart   TYPE zsdt0087-spart,
        mark(1),
      END OF tg_itens,


      BEGIN OF tg_itens_fr OCCURS 0,
        auart TYPE  zsdt0258-auart,
        inco1 TYPE  zsdt0258-inco1,
        inco2 TYPE  zsdt0258-inco2,
        inco3 TYPE  zsdt0258-inco3,
        inco4 TYPE  zsdt0258-inco4,
        ins   TYPE c,
      END OF tg_itens_fr.



*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.
*            lcl_alv_toolbar2  definition deferred.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_ITENS',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      g_custom_container2  TYPE REF TO cl_gui_custom_container,

      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      splitter             TYPE REF TO cl_gui_splitter_container,
      splitter2            TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,

      wa_style             TYPE lvc_s_styl,
      style                TYPE lvc_t_styl  WITH HEADER LINE,
      style2               TYPE lvc_t_styl WITH HEADER LINE.

*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.


** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl,

      tg_fields      TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_msg_ret     TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.

DATA: BEGIN OF gt_values OCCURS 0,
        domvalue_l TYPE domvalue_l,
        ddtext     TYPE val_text,
      END OF gt_values.

DATA: gs_variant_c TYPE disvariant,
      vg_screen.

*-US192810-08.10.2025-#192810-JT-inicio
DATA: lv_cadger TYPE char01,
      lv_zsd044 TYPE char01,
      lv_zsd079 TYPE char01,
      lv_zsd081 TYPE char01,
      lv_zsd087 TYPE char01.
*-US192810-08.10.2025-#192810-JT-fim

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_0               TYPE c VALUE '0',
           c_1               TYPE c VALUE '1',
           c_2               TYPE c VALUE '2',
           c_b               TYPE c VALUE 'B',
           c_s               TYPE c VALUE 'S',
           c_l               TYPE c VALUE 'L',
           c_x               TYPE c VALUE 'X',
           c_d               TYPE c VALUE 'D',
           c_k               TYPE c VALUE 'K',
           c_w               TYPE c VALUE 'W',
           c_f               TYPE c VALUE 'F',
           c_t               TYPE c VALUE 'T',
           c_i               TYPE c VALUE 'I',
           c_n               TYPE c VALUE 'N',
           c_h               TYPE c VALUE 'H',
           c_ag(2)           TYPE c VALUE 'AG',
           c_ne(2)           TYPE c VALUE 'NE',
           c_01(2)           TYPE c VALUE '01',
           c_30(2)           TYPE c VALUE '30',
           c_40(2)           TYPE c VALUE '40',
           c_50(4)           TYPE c VALUE '0050',
           c_76(2)           TYPE c VALUE '76',
           c_71(2)           TYPE c VALUE '71',
           c_72(2)           TYPE c VALUE '72',
           c_br(2)           TYPE c VALUE 'BR',
           c_lf(2)           TYPE c VALUE 'LF',
           c_lr(2)           TYPE c VALUE 'LR',
           c_z1(2)           TYPE c VALUE 'Z1',
           c_add(3)          TYPE c VALUE 'ADD',
           c_del(3)          TYPE c VALUE 'DEL',
           c_dg1(3)          TYPE c VALUE 'DG1',
           c_dg2(3)          TYPE c VALUE 'DG2',
           c_dummy_header(3) TYPE c VALUE '099',
           c_dummy_itens(3)  TYPE c VALUE '098',
           c_exit(4)         TYPE c VALUE 'EXIT',
           c_root(4)         TYPE c VALUE 'ROOT',
           c_minimizar(4)    TYPE c VALUE '@K2@',
           c_maximizar(4)    TYPE c VALUE '@K1@',
           c_back(4)         TYPE c VALUE 'BACK',
           c_save(4)         TYPE c VALUE 'SAVE',
           c_desat(5)        TYPE c VALUE 'DESAT',
           c_dmbtr(5)        TYPE c VALUE 'DMBTR',
           c_modif(5)        TYPE c VALUE 'MODIF',
           c_cancel(6)       TYPE c VALUE 'CANCEL',
           c_deldoc(6)       TYPE c VALUE 'DELDOC',
           c_dclick(6)       TYPE c VALUE 'DCLICK',
           c_search(6)       TYPE c VALUE 'SEARCH',
           c_atuali(6)       TYPE c VALUE 'ATUALI',
           c_add_msg(7)      TYPE c VALUE 'ADD_MSG',
           c_del_msg(7)      TYPE c VALUE 'DEL_MSG',
           c_clos_msg(8)     TYPE c VALUE 'CLOS_MSG',
           c_save_msg(8)     TYPE c VALUE 'SAVE_MSG',
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE',
           c_tela(4)         TYPE c VALUE '0102'.

*-US192810-08.10.2025-#192810-JT-inicio
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

*-------------------------------------------------------
*-bloco 1 ----------------------------------------------
*-------------------------------------------------------
  PARAMETERS: p_cadger RADIOBUTTON GROUP g0 USER-COMMAND g0 DEFAULT 'X'.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_mapa RADIOBUTTON GROUP   g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (60) TEXT-004 FOR FIELD p_mapa MODIF ID op1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_mcsmte RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-020 FOR FIELD p_mcsmte MODIF ID op1. "//Cadastro de Marca de Sementes
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_grpseg RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-021 FOR FIELD p_grpseg MODIF ID op1. "//Cadastro Grupo Mercadoria x Segmento
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_cult RADIOBUTTON   GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-028 FOR FIELD p_cult MODIF ID op1. "FF #178892 "//Cadastrar tipo "CULTIVAR"
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_venesp RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-032 FOR FIELD p_venesp MODIF ID op1. "FF #180571 "//Aprovadores para vendas especiais.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_pzsols RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-108 FOR FIELD p_pzsols MODIF ID op1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_nomcom RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-304 FOR FIELD p_nomcom MODIF ID op1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_prativ RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-305 FOR FIELD p_prativ MODIF ID op1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_formul RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-306 FOR FIELD p_formul MODIF ID op1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_classe RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-307 FOR FIELD p_classe MODIF ID op1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_alvo   RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-308 FOR FIELD p_alvo   MODIF ID op1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_aplic   RADIOBUTTON GROUP g11 MODIF ID op1.
    SELECTION-SCREEN COMMENT (79) TEXT-309 FOR FIELD p_aplic  MODIF ID op1.
  SELECTION-SCREEN END OF LINE.

*-------------------------------------------------------
*-bloco 2 ----------------------------------------------
*-------------------------------------------------------
  PARAMETERS: p_zsd044 RADIOBUTTON GROUP g0.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS  p_tp_ov RADIOBUTTON GROUP g12 MODIF ID op2.
    SELECTION-SCREEN COMMENT (51) TEXT-301 FOR FIELD p_tp_ov MODIF ID op2.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_cadmt RADIOBUTTON GROUP g12 MODIF ID op2.
    SELECTION-SCREEN COMMENT (79) TEXT-024 FOR FIELD p_cadmt MODIF ID op2. "FF #177784 "//Cadastro Parâmetros de Materiais
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_exmat RADIOBUTTON GROUP g12 MODIF ID op2.
    SELECTION-SCREEN COMMENT (79) TEXT-029 FOR FIELD p_exmat MODIF ID op2. "FF #180476 "//Usuário Exceção p/ Cadastro de Parâmetro de Materiais
  SELECTION-SCREEN END OF LINE.

*-------------------------------------------------------
*-bloco 3 ----------------------------------------------
*-------------------------------------------------------
  PARAMETERS: p_zsd079 RADIOBUTTON GROUP g0.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_caddia RADIOBUTTON GROUP g13 MODIF ID op3.
    SELECTION-SCREEN COMMENT (79) TEXT-025 FOR FIELD p_caddia MODIF ID op3. "FF #177784 "//Cadastro Limite de dias programação da roteirização
  SELECTION-SCREEN END OF LINE.
  PARAMETERS: p_null01 RADIOBUTTON GROUP g13 MODIF ID opx.

*-------------------------------------------------------
*-bloco 4 ----------------------------------------------
*-------------------------------------------------------
  PARAMETERS: p_zsd081 RADIOBUTTON GROUP g0.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_expri RADIOBUTTON GROUP g14 MODIF ID op4.
    SELECTION-SCREEN COMMENT (79) TEXT-030 FOR FIELD p_expri MODIF ID op4. "FF #180571 "//Cadastro de exceção para modificação de prioridade ZSDT0081
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_pridis RADIOBUTTON GROUP g14 MODIF ID op4.
    SELECTION-SCREEN COMMENT (79) TEXT-031 FOR FIELD p_pridis MODIF ID op4. "FF #180571 "//Usuarios p/ modificação prioridade distribuição ZSDT0081.
  SELECTION-SCREEN END OF LINE.

*-------------------------------------------------------
*-bloco 5 ----------------------------------------------
*-------------------------------------------------------
  PARAMETERS: p_zsd087 RADIOBUTTON GROUP g0.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS  p_tp_fr RADIOBUTTON GROUP g15 MODIF ID op5.
    SELECTION-SCREEN COMMENT (51) TEXT-302 FOR FIELD p_tp_fr MODIF ID op5.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_hora RADIOBUTTON  GROUP g15 MODIF ID op5.
    SELECTION-SCREEN COMMENT (79) TEXT-006 FOR FIELD p_hora MODIF ID op5.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_exprop RADIOBUTTON GROUP g15 MODIF ID op5.
    SELECTION-SCREEN COMMENT (79) TEXT-033 FOR FIELD p_exprop MODIF ID op5. "WB #169490 "//Cadastro de Usuário - Exceção Quantidade Proporcional ZSDT0087
  SELECTION-SCREEN END OF LINE.

*-------------------------------------------------------
*-bloco 6 ----------------------------------------------
*-------------------------------------------------------
  PARAMETERS: p_zsd112 RADIOBUTTON GROUP g0.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS  p_cd_pd RADIOBUTTON GROUP g16 MODIF ID op6 USER-COMMAND g16.
    SELECTION-SCREEN COMMENT (51) TEXT-002 FOR FIELD p_cd_pd MODIF ID op6.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS  p_cd_tr RADIOBUTTON GROUP g16 MODIF ID op6.
    SELECTION-SCREEN COMMENT (37) TEXT-003 FOR FIELD p_cd_tr MODIF ID op6.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_lote RADIOBUTTON  GROUP g16 MODIF ID op6.
    SELECTION-SCREEN COMMENT (79) TEXT-005 FOR FIELD p_lote MODIF ID op6.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 8.
    SELECTION-SCREEN COMMENT (18) TEXT-303 FOR FIELD p_vkorg MODIF ID op6.
    PARAMETERS: p_vkorg LIKE vbak-vkorg MODIF ID op6.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_dep RADIOBUTTON   GROUP g16 MODIF ID op6.
    SELECTION-SCREEN COMMENT (79) TEXT-007 FOR FIELD p_dep MODIF ID op6.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_frete RADIOBUTTON GROUP g16 MODIF ID op6.
    SELECTION-SCREEN COMMENT (79) TEXT-008 FOR FIELD p_frete MODIF ID op6.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_lcdate RADIOBUTTON GROUP g16 MODIF ID op6.
    SELECTION-SCREEN COMMENT (79) TEXT-009 FOR FIELD p_lcdate MODIF ID op6.
  SELECTION-SCREEN END OF LINE.

*-------------------------------------------------------
*-bloco 7 ----------------------------------------------
*-------------------------------------------------------
  PARAMETERS: p_inluft RADIOBUTTON GROUP g0.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_tpsmt RADIOBUTTON GROUP g17 MODIF ID op7.
    SELECTION-SCREEN COMMENT (79) TEXT-022 FOR FIELD p_tpsmt MODIF ID op7. "FF #177784 "//Determinar Tipo Semente p/ Borderô Recebimento LUFT
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_tpemb RADIOBUTTON GROUP g17 MODIF ID op7.
    SELECTION-SCREEN COMMENT (79) TEXT-023 FOR FIELD p_tpemb MODIF ID op7. "FF #177784 "//Determinar Embalagem p/ Borderô Recebimento LUFT
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_cadlp RADIOBUTTON GROUP g17 MODIF ID op7.
    SELECTION-SCREEN COMMENT (79) TEXT-026 FOR FIELD p_cadlp MODIF ID op7. "FF #177784 "//Ponto Coleta NF Saida - Integração LUFT
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_nfelf RADIOBUTTON GROUP g17 MODIF ID op7.
    SELECTION-SCREEN COMMENT (79) TEXT-027 FOR FIELD p_nfelf MODIF ID op7. "FF #177784 "//Parâmetros Envio NFe Entrada LUFT
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_luft RADIOBUTTON  GROUP g17 MODIF ID op7.
    SELECTION-SCREEN COMMENT (79) TEXT-106 FOR FIELD p_luft MODIF ID op7.
  SELECTION-SCREEN END OF LINE.

*-------------------------------------------------------
*-bloco 8 ----------------------------------------------
*-------------------------------------------------------
  PARAMETERS: p_insafr RADIOBUTTON GROUP g0.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_matenv RADIOBUTTON GROUP g18 MODIF ID op8.
    SELECTION-SCREEN COMMENT (79) TEXT-104 FOR FIELD p_matenv MODIF ID op8.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    PARAMETERS: p_gconta RADIOBUTTON GROUP g18 MODIF ID op8.
    SELECTION-SCREEN COMMENT (79) TEXT-105 FOR FIELD p_gconta MODIF ID op8.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.
*-US192810-08.10.2025-#192810-JT-fim

*  PARAMETERS: p_tp_ov RADIOBUTTON GROUP g1 USER-COMMAND g1 MODIF ID op1,
*              p_tp_fr RADIOBUTTON GROUP g1 MODIF ID op1.

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS  p_tp_ov RADIOBUTTON GROUP g1 MODIF ID opx.
*    SELECTION-SCREEN COMMENT 2(51) TEXT-301 FOR FIELD p_tp_ov MODIF ID opx.
*  SELECTION-SCREEN END OF LINE.
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS  p_tp_fr RADIOBUTTON GROUP g1 MODIF ID opx.
*    SELECTION-SCREEN COMMENT 2(51) TEXT-302 FOR FIELD p_tp_fr MODIF ID opx.
*  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS  p_cd_pd RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(51) TEXT-002 FOR FIELD p_cd_pd.
*  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS  p_cd_tr RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(37) TEXT-003 FOR FIELD p_cd_tr.
*  SELECTION-SCREEN END OF LINE.
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_mapa RADIOBUTTON GROUP g1 MODIF ID op1.
*    SELECTION-SCREEN COMMENT 2(60) TEXT-004 FOR FIELD p_mapa MODIF ID op1.
*  SELECTION-SCREEN END OF LINE.
*-CS2021000218-07.10.2022-#91712-JT-inicio
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_lote RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-005 FOR FIELD p_lote.
*  SELECTION-SCREEN END OF LINE.
*-CS2021000218-07.10.2022-#91712-JT-fim

" 12.06.2023 - 114475 - RAMON -------->
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_hora RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-006 FOR FIELD p_hora.
*  SELECTION-SCREEN END OF LINE.
" 12.06.2023 - 114475 - RAMON --------<

" 12.06.2023 - 114475 - RAMON -------->
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_dep RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-007 FOR FIELD p_dep.
*  SELECTION-SCREEN END OF LINE.
" 12.06.2023 - 114475 - RAMON --------<

* Inicio - Rubenilson - 15.08.24 - Cockpit Frete
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_frete RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-008 FOR FIELD p_frete.
*  SELECTION-SCREEN END OF LINE.
* Inicio - Rubenilson - 15.08.24 - Cockpit Frete

* Inicio - AOENNING - 16.09.24 - User Story 144014
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_lcdate RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-009 FOR FIELD p_lcdate.
*  SELECTION-SCREEN END OF LINE.
* Fim - AOENNING - 16.09.24 - User Story 144014

*-CS2025000249-12.03.2025-#170635-JT-inicio
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_mcsmte RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-020 FOR FIELD p_mcsmte. "//Cadastro de Marca de Sementes
*  SELECTION-SCREEN END OF LINE.
*-CS2025000249-12.03.2025-#170635-JT-fim

*-CS2025000249-26.03.2025-#170726-JT-inicio
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_grpseg RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-021 FOR FIELD p_grpseg. "//Cadastro Grupo Mercadoria x Segmento
*  SELECTION-SCREEN END OF LINE.
*-CS2025000249-26.03.2025-#170726-JT-fim

** inicio Descomentado para Subir para PRD WBARBOSA 27/06/25
*Projetos insumos 2025 - Tipo Semente
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_tpsmt RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-022 FOR FIELD p_tpsmt. "FF #177784 "//Determinar Tipo Semente p/ Borderô Recebimento LUFT
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_tpemb RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-023 FOR FIELD p_tpemb. "FF #177784 "//Determinar Embalagem p/ Borderô Recebimento LUFT
*  SELECTION-SCREEN END OF LINE.
** Fim Descomentado para Subir para PRD WBARBOSA  27/06/25

**Comentado para Liberar Versão para PRD WPP - Ini
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_cadmt RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-024 FOR FIELD p_cadmt. "FF #177784 "//Cadastro Parâmetros de Materiais
*  SELECTION-SCREEN END OF LINE.
*-Projetos insumos 2025 - Tipo Semente
**<<<------"169500 - NMS - INI------>>>
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_caddia RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-025 FOR FIELD p_caddia. "FF #177784 "//Cadastro Limite de dias programação da roteirização
*  SELECTION-SCREEN END OF LINE.
**<<<------"169500 - NMS - FIM------>>>

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_cadlp RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-026 FOR FIELD p_cadlp. "FF #177784 "//Ponto Coleta NF Saida - Integração LUFT
*  SELECTION-SCREEN END OF LINE.
*
*  "US 172204 - Envio NFe Entrada para LUFT - WPP --->>
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_nfelf RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-027 FOR FIELD p_nfelf. "FF #177784 "//Parâmetros Envio NFe Entrada LUFT
*  SELECTION-SCREEN END OF LINE.
"US 172204 - Envio NFe Entrada para LUFT - WPP <<---
**Comentado para Liberar Versão para PRD WPP - Fim

"FF #178892 - inicio
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_cult RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-028 FOR FIELD p_cult. "FF #178892 "//Cadastrar tipo "CULTIVAR"
*  SELECTION-SCREEN END OF LINE.
"FF #178892 - fim

"FF #180476 - inicio
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_exmat RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-029 FOR FIELD p_exmat. "FF #180476 "//Usuário Exceção p/ Cadastro de Parâmetro de Materiais
*  SELECTION-SCREEN END OF LINE.
"FF #180476 - fim

*"FF #180571 - inicio
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_expri RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-030 FOR FIELD p_expri. "FF #180571 "//Cadastro de exceção para modificação de prioridade ZSDT0081
*  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_pridis RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-031 FOR FIELD p_pridis. "FF #180571 "//Usuarios p/ modificação prioridade distribuição ZSDT0081.
*  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_venesp RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-032 FOR FIELD p_venesp. "FF #180571 "//Aprovadores para vendas especiais.
*  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_exprop RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-033 FOR FIELD p_exprop. "WB #169490 "//Cadastro de Usuário - Exceção Quantidade Proporcional ZSDT0087
*  SELECTION-SCREEN END OF LINE.

"FF #180571 - fim

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_matenv RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-104 FOR FIELD p_matenv.
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_gconta RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-105 FOR FIELD p_gconta.
*  SELECTION-SCREEN END OF LINE.

" 09.07.2025 - 168824 - RAMON -------->
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_luft RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-106 FOR FIELD p_luft.
*  SELECTION-SCREEN END OF LINE.
" 09.07.2025 - 168824 - RAMON --------<

"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: p_pzsols RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 2(79) TEXT-108 FOR FIELD p_pzsols.
*  SELECTION-SCREEN END OF LINE.
"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--

*-CS2021000218-07.10.2022-#91712-JT-inicio
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-010.
*  PARAMETERS: p_vkorg LIKE vbak-vkorg MODIF ID t1.
*SELECTION-SCREEN END OF BLOCK b2.
*-CS2021000218-07.10.2022-#91712-JT-fim

*-CS2021000218-07.10.2022-#91712-JT-inicio
AT SELECTION-SCREEN.
  IF p_lote = abap_true.
    IF p_vkorg IS INITIAL.
      MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    SELECT SINGLE vkorg
      INTO @DATA(l_vkorg)
      FROM tvko
     WHERE vkorg = @p_vkorg.

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH TEXT-102 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
*-US192810-08.10.2025-#192810-JT-inicio
    IF screen-group1 = 'OPX'.
      screen-invisible = 1.
      screen-input     = 0.
      screen-active    = 0.
    ENDIF.

    IF screen-group1 = 'OP1'.
      IF p_cadger = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'OP2'.
      IF p_zsd044 = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'OP3'.
      IF p_zsd079 = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'OP4'.
      IF p_zsd081 = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'OP5'.
      IF p_zsd087 = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'OP6'.
      IF p_zsd112 = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'OP7'.
      IF p_inluft = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'OP8'.
      IF p_insafr = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.
*-US192810-08.10.2025-#192810-JT-fim

    IF screen-group1 = 'T1'.
      IF p_lote = abap_true.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
      ELSE.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
*-CS2021000218-07.10.2022-#91712-JT-fim

START-OF-SELECTION.

*-CS2021000218-07.10.2022-#91712-JT-inicio
  CASE abap_true.
    WHEN p_cadger.

      CASE abap_true.
        WHEN p_mapa.
          "CALL TRANSACTION 'ZSD0001'.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0293'
                                WITH p_stcnam = 'ZSDT0293_OUT'
                                WITH p_scmant = '0143'
                                WITH p_title  = 'Cad. de Registro MAPA das Fábricas Fertilizantes'
                            AND RETURN.
        WHEN p_mcsmte.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0195'
                                WITH p_stcnam = 'ZMMT0195_OUT'
                                WITH p_scmant = '0287'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_title  = 'Cadastro de Marca de Sementes'
                            AND RETURN.
        WHEN p_grpseg.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0200'
                                WITH p_stcnam = 'ZMMT0200_OUT'
                                WITH p_scmant = '0292'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_title  = 'Cadastro Grupo Mercadoria x Segmento'
                            AND RETURN.
        WHEN p_cult.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0217'
                                WITH p_stcnam = 'ZMMT0217_OUT'
                                WITH p_scmant = '0312'
                                WITH p_title  = 'Parâmetro caraterística "CULTIVAR"'
                            AND RETURN.
        WHEN p_venesp.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0396'
                                WITH p_stcnam = 'ZSDT0396_OUT'
                                WITH p_scmant = '0315'
                                WITH p_title  = 'Aprovadores para vendas especiais'
                            AND RETURN.
        WHEN p_pzsols.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0423'
                                WITH p_stcnam = 'ZSDT0423'
                                WITH p_scmant = '0324'
                                WITH p_title  = TEXT-108
                                AND RETURN.
        WHEN p_nomcom.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0219'
                                WITH p_stcnam = 'ZMMT0219_OUT'
                                WITH p_scmant = '0342'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_nocwid = abap_true
                                WITH p_title  = TEXT-304
                                AND RETURN.
        WHEN p_prativ.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0220'
                                WITH p_stcnam = 'ZMMT0220_OUT'
                                WITH p_scmant = '0343'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_nocwid = abap_true
                                WITH p_title  = TEXT-305
                                AND RETURN.
        WHEN p_formul.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0221'
                                WITH p_stcnam = 'ZMMT0221_OUT'
                                WITH p_scmant = '0344'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_nocwid = abap_true
                                WITH p_title  = TEXT-306
                                AND RETURN.
        WHEN p_classe.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0222'
                                WITH p_stcnam = 'ZMMT0222_OUT'
                                WITH p_scmant = '0345'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_nocwid = abap_true
                                WITH p_title  = TEXT-307
                                AND RETURN.
        WHEN p_alvo.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0223'
                                WITH p_stcnam = 'ZMMT0223_OUT'
                                WITH p_scmant = '0346'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_nocwid = abap_true
                                WITH p_title  = TEXT-308
                                AND RETURN.
        WHEN p_aplic.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0224'
                                WITH p_stcnam = 'ZMMT0224_OUT'
                                WITH p_scmant = '0347'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_nocwid = abap_true
                                WITH p_title  = TEXT-309
                                AND RETURN.
      ENDCASE.

    WHEN p_zsd044.
      CASE abap_true.
        WHEN p_cadmt.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0384'
                             WITH p_stcnam = 'ZSDT0384_OUT'
                             WITH p_scmant = '0305'
                             WITH p_act_01 = 'Visualizar Log de Alteracoes'
                             WITH p_title  = 'Cadastro Materiais'
                         AND RETURN.

        WHEN p_exmat.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0394'
                                WITH p_stcnam = 'ZSDT0394_OUT'
                                WITH p_scmant = '0313'
                                WITH p_title  = 'Usuário Exceção p/ Cadastro de Parâmetro de Materiais'
                            AND RETURN.
      ENDCASE.

    WHEN p_zsd079.
      CASE abap_true.
        WHEN p_caddia. "Cadastro Limite de dias programação da roteirização
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0012'
                                WITH p_stcnam = 'ZSDT0012'
                                WITH p_scmant = '0298'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_title  = 'Cadastro Limite de dias'
                            AND RETURN.
      ENDCASE.

    WHEN p_zsd081.
      CASE abap_true.
        WHEN p_expri.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0395'
                             WITH p_stcnam = 'ZSDT0395_OUT'
                             WITH p_scmant = '0314'
                             WITH p_title  = 'Cadastro de exceção para modificação de prioridade ZSDT0081'
                         AND RETURN.
        WHEN p_pridis.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0398'
                                WITH p_stcnam = 'ZSDT0398_OUT'
                                WITH p_scmant = '0317'
                                WITH p_title  = 'Usuarios p/ modificação prioridade distribuição ZSDT0081'
                            AND RETURN.
      ENDCASE.

    WHEN p_zsd087.
      CASE abap_true.
        WHEN p_hora.
          "CALL TRANSACTION 'ZSDT0197'.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0333'
                                WITH p_stcnam = 'ZSDT0333'
                                WITH p_scmant = '0197'
                                WITH p_title  = 'Parametrização de Horário'
                            AND RETURN.
        WHEN p_exprop.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0408'
                                WITH p_stcnam = 'ZSDT0408_OUT'
                                WITH p_scmant = '0318'
                                WITH p_maxdel = '500'
                                WITH p_act_02 = TEXT-107 "// Visualizar Log de Alteracoes
                                WITH p_title  = TEXT-103 "// Cadastro de Usuário - Exceção Quantidade Proporcional
                             AND RETURN.
      ENDCASE.

    WHEN p_zsd112.
      CASE abap_true.
        WHEN p_cd_pd.
          "CALL TRANSACTION 'ZSDT0185'.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0281'
                                WITH p_stcnam = 'ZSDT0281_OUT'
                                WITH p_scmant = '0103'
                                WITH p_title  = 'Cad Excecao Centro - Vinc.Ped x OV - ZSDT0112'
                            AND RETURN.
        WHEN p_cd_tr.
          "CALL TRANSACTION 'ZSDT0186'.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0282'
                                WITH p_stcnam = 'ZSDT0282_OUT'
                                WITH p_scmant = '0105'
                                WITH p_title  = 'Cadastro de Transportadora - ZSDT0112'
                            AND RETURN.
        WHEN p_lote.
          SUBMIT zsdr0140 WITH p_vkorg = p_vkorg
                           AND RETURN.
        WHEN p_dep. "Cadastro Depósito
          "CALL TRANSACTION 'ZSDT0335'.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0335'
                                WITH p_stcnam = 'ZSDT0335'
                                WITH p_scmant = '0210'
                                WITH p_title  = 'Cadastro Depósito'
                            AND RETURN.
        WHEN p_frete.
          CALL TRANSACTION 'ZSDT0216'."SM30
        WHEN p_lcdate.
          "CALL TRANSACTION 'ZSDT0340'.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0348'
                                WITH p_stcnam = 'ZSDT0348'
                                WITH p_scmant = '0256'
                                WITH p_title  = 'Transação exceção ZSDT0112'
                            AND RETURN.
      ENDCASE.

    WHEN p_inluft.
      CASE abap_true.
        WHEN p_tpsmt.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0208'
                                WITH p_stcnam = 'ZMMT0208_OUT'
                                WITH p_scmant = '0301'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_title  = 'Cadastro Tipo semente'
                            AND RETURN.
        WHEN p_tpemb.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0209'
                                WITH p_stcnam = 'ZMMT0209_OUT'
                                WITH p_scmant = '0302'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_title  = 'Cadastro Embalagem'
                            AND RETURN.
        WHEN p_cadlp.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0386'
                                WITH p_stcnam = 'ZSDT0386_OUT'
                                WITH p_scmant = '0307'
                                WITH p_act_01 = 'Visualizar Log de Alteracoes'
                                WITH p_title  = 'Ponto Coleta NF Saida - Integração LUFT'
                            AND RETURN.
        WHEN p_nfelf.
          SUBMIT zregister_data WITH p_db_tab = 'ZMMT0214'
                                WITH p_stcnam = 'ZMMT0214'
                                WITH p_scmant = '0309'
                                WITH p_title  = 'Parâmetros Envio NF-e Entrada LUFT'
                                WITH p_db_01  = 'ZMMT0215'
                                WITH p_stc_01 = 'ZMMT0215'
                                WITH p_sc_01  = '0310'
                                WITH p_ti_01  = 'Filial Destino - Envio NFe Entrada LUFT'
                                WITH p_db_03  = 'ZMMT0216'
                                WITH p_stc_03 = 'ZMMT0216'
                                WITH p_sc_03  = '0311'
                                WITH p_ti_03  = 'NCM - Envio NFe Entrada LUFT'
                            AND RETURN.
        WHEN p_luft.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0419'
                                WITH p_stcnam = 'ZSDT0419'
                                WITH p_scmant = '322'
                                WITH p_title  = TEXT-106
                             AND RETURN.
      ENDCASE.

    WHEN p_insafr.
      CASE abap_true.
        WHEN p_matenv.
          SUBMIT zregister_data WITH p_db_tab = 'ZSDT0417'
                                WITH p_stcnam = 'ZSDT0417_OUT'
                                WITH p_scmant = '0321'
                                WITH p_title  = TEXT-104
                                WITH p_nosave = abap_true
                             AND RETURN.
        WHEN p_gconta.
          CALL TRANSACTION 'ZSDT0355'. "SM30
      ENDCASE.

  ENDCASE.
*-CS2021000218-07.10.2022-#91712-JT-fim

*  CASE abap_true.
*    WHEN p_cd_pd.
*      "CALL TRANSACTION 'ZSDT0185'.
*
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0281'
*                            WITH p_stcnam = 'ZSDT0281_OUT'
*                            WITH p_scmant = '0103'
*                            WITH p_title  = 'Cad Excecao Centro - Vinc.Ped x OV - ZSDT0112'
*                        AND RETURN.

*    WHEN p_cd_tr.
*      "CALL TRANSACTION 'ZSDT0186'.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0282'
*                            WITH p_stcnam = 'ZSDT0282_OUT'
*                            WITH p_scmant = '0105'
*                            WITH p_title  = 'Cadastro de Transportadora - ZSDT0112'
*                        AND RETURN.

*    WHEN p_mapa.
*      "CALL TRANSACTION 'ZSD0001'.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0293'
*                            WITH p_stcnam = 'ZSDT0293_OUT'
*                            WITH p_scmant = '0143'
*                            WITH p_title  = 'Cad. de Registro MAPA das Fábricas Fertilizantes'
*                        AND RETURN.

*-CS2021000218-07.10.2022-#91712-JT-inicio
*    WHEN p_lote.
*      SUBMIT zsdr0140 WITH p_vkorg = p_vkorg
*                       AND RETURN.
*-CS2021000218-07.10.2022-#91712-JT-fimo

  " 12.06.2023 - 114475 - RAMON -------->
*    WHEN p_hora.
*      "CALL TRANSACTION 'ZSDT0197'.
*
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0333'
*                            WITH p_stcnam = 'ZSDT0333'
*                            WITH p_scmant = '0197'
*                            WITH p_title  = 'Parametrização de Horário'
*                        AND RETURN.
  " 12.06.2023 - 114475 - RAMON --------<

*    WHEN p_dep. "Cadastro Depósito
*      "CALL TRANSACTION 'ZSDT0335'.
*
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0335'
*                            WITH p_stcnam = 'ZSDT0335'
*                            WITH p_scmant = '0210'
*                            WITH p_title  = 'Cadastro Depósito'
*                        AND RETURN.

* Inicio - Rubenilson - 15.08.24 - Cockpit Frete
*    WHEN p_frete.
*      CALL TRANSACTION 'ZSDT0216'."SM30
*
* Fim - Rubenilson - 15.08.24 - Cockpit Frete

*    WHEN p_lcdate.
*      "CALL TRANSACTION 'ZSDT0340'.
*
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0348'
*                            WITH p_stcnam = 'ZSDT0348'
*                            WITH p_scmant = '0256'
*                            WITH p_title  = 'Transação exceção ZSDT0112'
*                        AND RETURN.

*-CS2025000249-12.03.2025-#170635-JT-inicio
*    WHEN p_mcsmte.
*      SUBMIT zregister_data WITH p_db_tab = 'ZMMT0195'
*                            WITH p_stcnam = 'ZMMT0195_OUT'
*                            WITH p_scmant = '0287'
*                            WITH p_act_01 = 'Visualizar Log de Alteracoes'
*                            WITH p_title  = 'Cadastro de Marca de Sementes'
*                        AND RETURN.
*-CS2025000249-12.03.2025-#170635-JT-fim

*-CS2025000249-26.03.2025-#170726-JT-inicio
*    WHEN p_grpseg.
*      SUBMIT zregister_data WITH p_db_tab = 'ZMMT0200'
*                            WITH p_stcnam = 'ZMMT0200_OUT'
*                            WITH p_scmant = '0292'
*                            WITH p_act_01 = 'Visualizar Log de Alteracoes'
*                            WITH p_title  = 'Cadastro Grupo Mercadoria x Segmento'
*                        AND RETURN.
*-CS2025000249-26.03.2025-#170726-JT-fim

** inicio Descomentado para Subir para PRD WBARBOSA 27/06/25
** Projetos insumos 2025 - Tipo Semente
*    WHEN p_tpsmt.
*      SUBMIT zregister_data WITH p_db_tab = 'ZMMT0208'
*                            WITH p_stcnam = 'ZMMT0208_OUT'
*                            WITH p_scmant = '0301'
*                            WITH p_act_01 = 'Visualizar Log de Alteracoes'
*                            WITH p_title  = 'Cadastro Tipo semente'
*                        AND RETURN.

*    WHEN p_tpemb.
*      SUBMIT zregister_data WITH p_db_tab = 'ZMMT0209'
*                            WITH p_stcnam = 'ZMMT0209_OUT'
*                            WITH p_scmant = '0302'
*                            WITH p_act_01 = 'Visualizar Log de Alteracoes'
*                            WITH p_title  = 'Cadastro Embalagem'
*                        AND RETURN.
** fim Descomentado para Subir para PRD WBARBOSA 27/06/25

*    WHEN p_cadmt.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0384'
*                         WITH p_stcnam = 'ZSDT0384_OUT'
*                         WITH p_scmant = '0305'
*                         WITH p_act_01 = 'Visualizar Log de Alteracoes'
*                         WITH p_title  = 'Cadastro Materiais'
*                     AND RETURN.
*-Projetos insumos 2025 - Tipo Semente
**<<<------"169500 - NMS - INI------>>>
*    WHEN p_caddia. "Cadastro Limite de dias programação da roteirização
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0012'
*                            WITH p_stcnam = 'ZSDT0012'
*                            WITH p_scmant = '0298'
*                            WITH p_act_01 = 'Visualizar Log de Alteracoes'
*                            WITH p_title  = 'Cadastro Limite de dias'
*                        AND RETURN.
**<<<------"169500 - NMS - FIM------>>>
  " Envio NF-e Saida para CD - LUFT
*    WHEN p_cadlp.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0386'
*                            WITH p_stcnam = 'ZSDT0386_OUT'
*                            WITH p_scmant = '0307'
*                            WITH p_act_01 = 'Visualizar Log de Alteracoes'
*                            WITH p_title  = 'Ponto Coleta NF Saida - Integração LUFT'
*                        AND RETURN.

  "US 172204 - Envio NFe Entrada para LUFT - WPP --->>
*    WHEN p_nfelf.
*      SUBMIT zregister_data WITH p_db_tab = 'ZMMT0214'
*                            WITH p_stcnam = 'ZMMT0214'
*                            WITH p_scmant = '0309'
*                            WITH p_title  = 'Parâmetros Envio NF-e Entrada LUFT'
*
*                            WITH p_db_01  = 'ZMMT0215'
*                            WITH p_stc_01 = 'ZMMT0215'
*                            WITH p_sc_01  = '0310'
*                            WITH p_ti_01  = 'Filial Destino - Envio NFe Entrada LUFT'
*
*                            WITH p_db_03  = 'ZMMT0216'
*                            WITH p_stc_03 = 'ZMMT0216'
*                            WITH p_sc_03  = '0311'
*                            WITH p_ti_03  = 'NCM - Envio NFe Entrada LUFT'
*
*                        AND RETURN.
  "US 172204 - Envio NFe Entrada para LUFT - WPP <<---

  "FF #178892 - inicio
*    WHEN p_cult.
*      SUBMIT zregister_data WITH p_db_tab = 'ZMMT0217'
*                            WITH p_stcnam = 'ZMMT0217_OUT'
*                            WITH p_scmant = '0312'
*                            WITH p_title  = 'Parâmetro caraterística "CULTIVAR"'
*                        AND RETURN.
  "FF #178892 - fim
  "FF #180476 - inicio
*    WHEN p_exmat.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0394'
*                            WITH p_stcnam = 'ZSDT0394_OUT'
*                            WITH p_scmant = '0313'
*                            WITH p_title  = 'Usuário Exceção p/ Cadastro de Parâmetro de Materiais'
*                        AND RETURN.
  "FF #180476 - fim

*    WHEN p_matenv.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0417'
*                            WITH p_stcnam = 'ZSDT0417_OUT'
*                            WITH p_scmant = '0321'
*                            WITH p_title  = TEXT-104
*                            WITH p_nosave = abap_true
*                         AND RETURN.

*    WHEN p_gconta.
*      CALL TRANSACTION 'ZSDT0355'. "SM30

  " 09.07.2025 - 168824 - RAMON -------->
*    WHEN p_luft.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0419'
*                            WITH p_stcnam = 'ZSDT0419'
*                            WITH p_scmant = '322'
*                            WITH p_title  = TEXT-106
*                         AND RETURN.
  " 09.07.2025 - 168824 - RAMON --------<

*      FF #180571 - inicio
*    WHEN p_expri.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0395'
*                         WITH p_stcnam = 'ZSDT0395_OUT'
*                         WITH p_scmant = '0314'
*                         WITH p_title  = 'Cadastro de exceção para modificação de prioridade ZSDT0081'
*                     AND RETURN.

*    WHEN p_venesp.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0396'
*                            WITH p_stcnam = 'ZSDT0396_OUT'
*                            WITH p_scmant = '0315'
*                            WITH p_title  = 'Aprovadores para vendas especiais'
*                        AND RETURN.
*
*    WHEN p_pridis.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0398'
*                            WITH p_stcnam = 'ZSDT0398_OUT'
*                            WITH p_scmant = '0317'
*                            WITH p_title  = 'Usuarios p/ modificação prioridade distribuição ZSDT0081'
*                        AND RETURN.
  "FF #180571 - fim


* "// WBARBOSA #169490 "//Cadastro de Usuário - Exceção Quantidade Proporcional ZSDT0087
*    WHEN p_exprop.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0408'
*                            WITH p_stcnam = 'ZSDT0408_OUT'
*                            WITH p_scmant = '0318'
*                            WITH p_maxdel = '500'
*                            WITH p_act_02 = TEXT-107 "// Visualizar Log de Alteracoes
*                            WITH p_title  = TEXT-103 "// Cadastro de Usuário - Exceção Quantidade Proporcional
*                         AND RETURN.
* "// WBARBOSA #169490 "//Cadastro de Usuário - Exceção Quantidade Proporcional ZSDT0087

*    WHEN p_matenv.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0417'
*                          WITH p_stcnam = 'ZSDT0417_OUT'
*                          WITH p_scmant = '0321'
*                          WITH p_title  = TEXT-104
*                          WITH p_nosave = abap_true
*                       AND RETURN.

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
*    WHEN p_pzsols.
*      SUBMIT zregister_data WITH p_db_tab = 'ZSDT0423'
*                            WITH p_stcnam = 'ZSDT0423'
*                            WITH p_scmant = '0324'
*                            WITH p_title  = TEXT-108
*                            AND RETURN.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--
*  ENDCASE.

  PERFORM fm_busca_dados.


END-OF-SELECTION.

  IF ( p_tp_ov IS NOT INITIAL AND p_zsd044 = abap_true ) OR "*-CS2021000218-07.10.2022-#91712-JT
     ( p_tp_fr IS NOT INITIAL AND p_zsd087 = abap_true ).   "*-CS2021000218-07.10.2022-#91712-JT
    CALL SCREEN 0100.
  ENDIF.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


CLASS lcl_event_handler_fr DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed_fr FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished_fr FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
ENDCLASS.



*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_data_changed_finished.

    "SORT TG_ITENS BY  MATKL TPSIM AUART.
** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


CLASS lcl_event_handler_fr IMPLEMENTATION.

  METHOD on_data_changed_fr.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wg_good_cells)
      WHERE  fieldname EQ 'AUART'.

      LOOP AT tg_itens_fr INTO DATA(wg_itens_fr).

        CHECK wg_good_cells-row_id EQ sy-tabix.

        CASE wg_good_cells-fieldname.
          WHEN 'AUART'.

            READ TABLE tg_itens_fr INTO DATA(_itens_fr) WITH KEY auart = wg_good_cells-value.
            IF sy-subrc EQ 0.
              MESSAGE i836(sd) WITH 'Tipos O.V. informada já existe!'.
              EXIT.
            ENDIF.

        ENDCASE.
      ENDLOOP.

    ENDLOOP.

    CALL METHOD grid2->refresh_table_display( is_stable = wa_stable ).

  ENDMETHOD.

  METHOD on_data_changed_finished_fr.

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.

*    IF WG_ACAO NE C_MODIF.
*      WL_DESACTIVE = 1.
*    ENDIF.
    CLEAR wl_desactive.
    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                  "on_toolbar


  METHOD handle_user_command.
    DATA: tl_itens_aux    LIKE TABLE OF tg_itens,
          tl_itens_fr_aux LIKE TABLE OF tg_itens_fr,
          wl_itens        LIKE LINE OF tg_itens,
          wl_itens_fr     LIKE LINE OF tg_itens_fr,
          wl_lines        TYPE sy-tabix.


    REFRESH: tl_itens_aux, tl_itens_fr_aux.

    CASE e_ucomm.
      WHEN c_add.
        IF p_tp_ov IS NOT INITIAL.

          tl_itens_aux[] = tg_itens[].
          REFRESH: tg_itens.
          LOOP AT tl_itens_aux INTO wl_itens.
            APPEND wl_itens TO tg_itens.
          ENDLOOP.
          CLEAR: wl_itens.
          APPEND wl_itens TO tg_itens.

        ELSE.

*          TL_ITENS_FR_AUX[] = TG_ITENS_FR[].
*          REFRESH: TG_ITENS_FR.
*          LOOP AT TL_ITENS_FR_AUX INTO WL_ITENS_FR.
*            APPEND WL_ITENS_FR TO TG_ITENS_FR.
*          ENDLOOP.
*          CLEAR: WL_ITENS_FR.
          wl_itens_fr-ins  = abap_true.
          APPEND wl_itens_fr TO tg_itens_fr.

        ENDIF.


      WHEN c_del.

        IF p_tp_ov IS NOT INITIAL.

          CALL METHOD grid1->get_selected_cells
            IMPORTING
              et_cell = tg_selectedcell.

          LOOP AT tg_selectedcell INTO wg_selectedcell.
            DELETE tg_itens INDEX wg_selectedcell-row_id-index.
          ENDLOOP.
        ELSE.

          CALL METHOD grid2->get_selected_cells
            IMPORTING
              et_cell = tg_selectedcell.

          LOOP AT tg_selectedcell INTO wg_selectedcell.
            DELETE tg_itens_fr INDEX wg_selectedcell-row_id-index.
          ENDLOOP.
        ENDIF.
    ENDCASE.

    IF p_tp_ov IS NOT INITIAL.
      CALL METHOD grid1->refresh_table_display( is_stable = wa_stable ).
    ELSE.
      CALL METHOD grid2->refresh_table_display( is_stable = wa_stable ).
    ENDIF.

  ENDMETHOD.                    "zm_handle_user_command
ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
