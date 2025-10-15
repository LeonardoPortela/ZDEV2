*&---------------------------------------------------------------------*
*&  Include           ZSDR0153_TOP
*&---------------------------------------------------------------------*
CONSTANTS: BEGIN OF c_tab_strip_cliente,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_CLI_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_CLI_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_CLI_FC3',
             tab4 LIKE sy-ucomm VALUE 'TAB_STRIP_CLI_FC4',"Rubenilson  - 19.06.24 - #150257
             tab5 LIKE sy-ucomm VALUE 'TAB_STRIP_CLI_FC5',"Rubenilson  - 19.06.24 - #150257
           END OF c_tab_strip_cliente.

CONTROLS: tab_strip_cliente TYPE TABSTRIP,
          tctrl_zahlwege    TYPE TABLEVIEW USING SCREEN 0104.

DATA: BEGIN OF g_tab_strip_cli,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZSDR0153',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_cliente-tab1,
      END OF g_tab_strip_cli.


*******************************************************************************************
* Types
*******************************************************************************************
TYPES: BEGIN OF ty_zsdt0317.
         INCLUDE STRUCTURE zsdt0317.
TYPES:
         modif   TYPE c,
         criado  TYPE c,
         celltab TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0317.

*** Inicio - Rubenilson  - 19.06.24 - #150257
TYPES: BEGIN OF ty_zsdt0341.
         INCLUDE STRUCTURE zsdt0341.
TYPES:
         modif   TYPE c,
         criado  TYPE c,
         celltab TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0341.

TYPES: BEGIN OF ty_zsdt0342.
         INCLUDE STRUCTURE zsdt0342.
TYPES:
         modif   TYPE c,
         criado  TYPE c,
         celltab TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0342.
*** Fim - Rubenilson  - 19.06.24 - #150257

TYPES: BEGIN OF ty_zsdt0319.
         INCLUDE STRUCTURE zsdt0319.
TYPES:
         vtext   TYPE tvtwt-vtext,
         modif   TYPE c,
         criado  TYPE c,
         celltab TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0319.


TYPES: BEGIN OF ty_zsdt0320.
         INCLUDE STRUCTURE zsdt0320.
TYPES:
         vtext   TYPE tspat-vtext,
         modif   TYPE c,
         criado  TYPE c,
         celltab TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0320.


TYPES: BEGIN OF ty_zsdt0321.
         INCLUDE STRUCTURE zsdt0321.
TYPES:   vtext   TYPE tpart-vtext,
         modif   TYPE c,
         criado  TYPE c,
         celltab TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0321.

TYPES: BEGIN OF ty_zsdt0322.
         INCLUDE STRUCTURE zsdt0322.
TYPES:
         modif   TYPE c,
         criado  TYPE c,
         celltab TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0322.

DATA: BEGIN OF a042z OCCURS 10,
        zlsch    LIKE t042z-zlsch,   " Zahlweg
        text1    LIKE t042z-text1,   " Bedeutung des Zahlwegs
        xselk(1) TYPE c,             " KZ: X=Zahlweg ausgewaehlt
      END OF a042z.

DATA: BEGIN OF e042z OCCURS 10,
        zlsch    LIKE t042z-zlsch,   " Zahlweg
        text1    LIKE t042z-text1,   " Bedeutung des Zahlwegs
        xselk(1) TYPE c,             " KZ: X=Zahlweg ausgewaehlt
      END OF e042z.



*------- SORTTAB Hilfstabelle fuer Sortierung --------------------------
DATA: BEGIN OF sorttab OCCURS 10,
        arg(1) TYPE c,             " Sortierfeld
      END OF sorttab.

*******************************************************************************************
* DATA
*******************************************************************************************
DATA: t_fieldcatalog        TYPE lvc_t_fcat,
      t_fieldcatalog_0102   TYPE lvc_t_fcat,
      t_fieldcatalog_0103   TYPE lvc_t_fcat,
      t_fieldcatalog_0103_1 TYPE lvc_t_fcat,
      t_fieldcatalog_0103_2 TYPE lvc_t_fcat,
      t_fieldcatalog_0103_3 TYPE lvc_t_fcat,
      t_fieldcatalog_0105   TYPE lvc_t_fcat,"Rubenilson  - 19.06.24 - #150257
      t_fieldcatalog_0106   TYPE lvc_t_fcat,"Rubenilson  - 19.06.24 - #150257
      w_layout              TYPE lvc_s_layo,
      w_layout_0102         TYPE lvc_s_layo,
      w_layout_0103_1       TYPE lvc_s_layo,
      w_layout_0103_2       TYPE lvc_s_layo,
      w_layout_0103_3       TYPE lvc_s_layo,
      w_layout_0103         TYPE lvc_s_layo,
      w_layout_0105         TYPE lvc_s_layo,"Rubenilson  - 19.06.24 - #150257
      w_layout_0106         TYPE lvc_s_layo,"Rubenilson  - 19.06.24 - #150257
      pt_exclude            TYPE ui_functions,
      w_stable              TYPE lvc_s_stbl VALUE 'XX',
      gt_f4                 TYPE lvc_t_f4 WITH HEADER LINE.


DATA: tree1                     TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar                TYPE REF TO cl_gui_toolbar,
      g_container               TYPE scrfname VALUE 'CC_GP_CONTAS',
      g_container_0102          TYPE scrfname VALUE 'CC_EMPRESA',
      g_container_0103          TYPE scrfname VALUE 'CC_DISTRIBUICAO',
      g_container_0103_1        TYPE scrfname VALUE 'CC_ATIVIDADE',
      g_container_0103_2        TYPE scrfname VALUE 'CC_FUNCAO',
      g_container_0103_3        TYPE scrfname VALUE 'CC_ADICIONAIS',
      g_container_0105          TYPE scrfname VALUE 'CC_EMPRESA_FORNEC',"Rubenilson  - 19.06.24 - #150257
      g_container_0106          TYPE scrfname VALUE 'CC_DADOS_COMPRA',"Rubenilson  - 19.06.24 - #150257
      obj_dyndoc_id             TYPE REF TO cl_dd_document,
      g_custom_container        TYPE REF TO cl_gui_custom_container,
      g_custom_container_0102   TYPE REF TO cl_gui_custom_container,
      g_custom_container_0103   TYPE REF TO cl_gui_custom_container,
      g_custom_container_0103_1 TYPE REF TO cl_gui_custom_container,
      g_custom_container_0103_2 TYPE REF TO cl_gui_custom_container,
      g_custom_container_0103_3 TYPE REF TO cl_gui_custom_container,
      g_custom_container_0105   TYPE REF TO cl_gui_custom_container,"Rubenilson  - 19.06.24 - #150257
      g_custom_container_0106   TYPE REF TO cl_gui_custom_container,"Rubenilson  - 19.06.24 - #150257
      g_grid_grupo_c            TYPE REF TO cl_gui_alv_grid,
      g_grid_empresa            TYPE REF TO cl_gui_alv_grid,
      g_grid_empresa_fornec     TYPE REF TO cl_gui_alv_grid,"Rubenilson  - 19.06.24 - #150257
      g_grid_distrib            TYPE REF TO cl_gui_alv_grid,
      g_grid_atividade          TYPE REF TO cl_gui_alv_grid,
      g_grid_parceiro           TYPE REF TO cl_gui_alv_grid,
      g_grid_adicionais         TYPE REF TO cl_gui_alv_grid,
      g_grid_dados_compra       TYPE REF TO cl_gui_alv_grid,"Rubenilson  - 19.06.24 - #150257
      w_tool                    TYPE stb_button,
      t_estilo                  TYPE lvc_t_styl,
      t_del_rows                TYPE lvc_t_row,
      w_del_rows                TYPE lvc_s_row.


DATA: t_ktokd TYPE TABLE OF ty_zsdt0317.

DATA: refe1(8) TYPE p,
      refe2(8) TYPE p,
      tfill    TYPE i,
      index    TYPE i.


DATA: loopc         TYPE i,             " Hilfsfeld Listbildblättern
      save_loopc(2) TYPE p,             " Hilfsfeld Listbildblättern
      lflag(1)      TYPE c,             " Flag 'Zeilenselektion'
      lindex        TYPE i,             " Zeilenzähler Listbild
      zwcnt(2)      TYPE p VALUE 0.

DATA: g_ebpp_active(1)    TYPE c,   " X = EBPP ist aktiv  " \TP 509532
      g_ebpp_zahlwege(20) TYPE c.   " Alle EBPP-Zahlwege  " \TP 509532


DATA: gva_xasel TYPE rf02d-xasel,
      gva_azsch TYPE rf02d-azsch,
      gva_aztxt TYPE rf02d-aztxt,
      gva_xesel TYPE rf02d-xesel,
      gva_ezsch TYPE rf02d-ezsch,
      gva_eztxt TYPE rf02d-eztxt.



*----------------------------------------------------------------------*
* Variaveis Globais
*----------------------------------------------------------------------*
DATA: vg_verifica_selecao_gp TYPE sy-subrc,
      vg_verifica_selecao_ep TYPE sy-subrc,
      vg_troca               TYPE sy-subrc,
      vg_tab(4)              TYPE c,
      vg_bukrs               TYPE zsdt0317-bukrs,
      vg_usuario_cancel      TYPE sy-uname,
      vg_data_cancel         TYPE sy-datum,
      vg_hora_cancel         TYPE sy-uzeit,
      "vg_erro(1)             TYPE c,
      var_answer             TYPE c.


*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
DATA: c_x           TYPE c LENGTH 01 VALUE 'X',
      vg_dynnr_xxxx TYPE sydynnr.

*----------------------------------------------------------------------*
* Variáveis de Seleção de Consultas
*----------------------------------------------------------------------*
DATA: it_grupo_contas           TYPE TABLE OF zsdt0317 WITH HEADER LINE,
      wa_grupo_contas           TYPE zsdt0317,

      it_grupo_contas_alv       TYPE TABLE OF ty_zsdt0317 WITH HEADER LINE,
      wa_grupo_contas_alv       TYPE ty_zsdt0317,

      it_grupo_contas_alv_aux   TYPE TABLE OF ty_zsdt0317 WITH HEADER LINE,
      wa_grupo_contas_alv_aux   TYPE ty_zsdt0317,

      it_grupo_contas_del       TYPE TABLE OF ty_zsdt0317 WITH HEADER LINE,
      wa_grupo_contas_del       TYPE ty_zsdt0317,
      it_canal_dist             TYPE TABLE OF zsdt0319 WITH HEADER LINE,
      wa_canal_dist             TYPE zsdt0319,
      it_canal_dist_alv         TYPE TABLE OF ty_zsdt0319 WITH HEADER LINE,
      wa_canal_dist_alv         TYPE ty_zsdt0319,
      it_canal_dist_alv_aux     TYPE TABLE OF ty_zsdt0319 WITH HEADER LINE,
      wa_canal_dist_alv_aux     TYPE ty_zsdt0319,

      it_canal_dist_del         TYPE TABLE OF ty_zsdt0319 WITH HEADER LINE,
      wa_canal_dist_del         TYPE ty_zsdt0319,
      it_set_atv                TYPE TABLE OF zsdt0320 WITH HEADER LINE,
      wa_set_atv                TYPE zsdt0320,
      it_set_atv_alv            TYPE TABLE OF ty_zsdt0320 WITH HEADER LINE,
      wa_set_atv_alv            TYPE ty_zsdt0320,

      it_set_atv_alv_aux        TYPE TABLE OF ty_zsdt0320 WITH HEADER LINE,
      wa_set_atv_alv_aux        TYPE ty_zsdt0320,

      it_set_atv_del            TYPE TABLE OF ty_zsdt0320 WITH HEADER LINE,
      wa_set_atv_del            TYPE ty_zsdt0320,
      it_parceiros              TYPE TABLE OF zsdt0321 WITH HEADER LINE,
      wa_parceiros              TYPE zsdt0321,
      it_parceiros_alv          TYPE TABLE OF ty_zsdt0321 WITH HEADER LINE,
      wa_parceiros_alv          TYPE ty_zsdt0321,
      it_parceiros_del          TYPE TABLE OF ty_zsdt0321 WITH HEADER LINE,
      wa_parceiros_del          TYPE ty_zsdt0321,
      it_area_venda             TYPE TABLE OF zsdt0322 WITH HEADER LINE,
      wa_area_venda             TYPE zsdt0322,
      it_area_venda_alv         TYPE TABLE OF ty_zsdt0322 WITH HEADER LINE,
      wa_area_venda_alv         TYPE ty_zsdt0322,

      it_area_venda_alv_aux     TYPE TABLE OF ty_zsdt0322 WITH HEADER LINE,
      wa_area_venda_alv_aux     TYPE ty_zsdt0322,

      it_area_venda_del         TYPE TABLE OF ty_zsdt0322 WITH HEADER LINE,
      wa_area_venda_del         TYPE ty_zsdt0322,

      it_empresa_fornec         TYPE TABLE OF zsdt0341 WITH HEADER LINE,
      wa_empresa_fornec         TYPE zsdt0341,

      it_empresa_fornec_alv     TYPE TABLE OF ty_zsdt0341 WITH HEADER LINE,"Rubenilson  - 19.06.24 - #150257
      wa_empresa_fornec_alv     TYPE ty_zsdt0341,"Rubenilson  - 19.06.24 - #150257

      it_empresa_fornec_alv_aux TYPE TABLE OF ty_zsdt0341 WITH HEADER LINE,"Rubenilson  - 19.06.24 - #150257
      wa_empresa_fornec_alv_aux TYPE ty_zsdt0341,"Rubenilson  - 19.06.24 - #150257

      it_empresa_fornec_del     TYPE TABLE OF ty_zsdt0341 WITH HEADER LINE,"Rubenilson  - 19.06.24 - #150257
      wa_empresa_fornec_del     TYPE ty_zsdt0341,"Rubenilson  - 19.06.24 - #150257

      it_dados_compra        TYPE TABLE OF zsdt0342 WITH HEADER LINE,"Rubenilson  - 19.06.24 - #150257
      wa_dados_compra         TYPE zsdt0342,"Rubenilson  - 19.06.24 - #150257

      it_dados_compra_alv     TYPE TABLE OF ty_zsdt0342 WITH HEADER LINE,"Rubenilson  - 19.06.24 - #150257
      wa_dados_compra_alv     TYPE ty_zsdt0342,"Rubenilson  - 19.06.24 - #150257

      it_dados_compra_alv_aux TYPE TABLE OF ty_zsdt0342 WITH HEADER LINE,"Rubenilson  - 19.06.24 - #150257
      wa_dados_compra_alv_aux TYPE ty_zsdt0342,"Rubenilson  - 19.06.24 - #150257

      it_dados_compra_del     TYPE TABLE OF ty_zsdt0342 WITH HEADER LINE,"Rubenilson  - 19.06.24 - #150257
      wa_dados_compra_del     TYPE ty_zsdt0342."Rubenilson  - 19.06.24 - #150257


DATA:
  wa_msg(30),
  i_show(1).

DATA: it_msg_ret      TYPE TABLE OF zsdt0324 WITH HEADER LINE,
      wa_msg_ret      LIKE LINE OF it_msg_ret,
      wg_mensagem(30),
      x_field(30).
