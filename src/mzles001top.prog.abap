*&---------------------------------------------------------------------*
*& Include MZLES001TOP                    PoolMóds.        SAPMZLES001
*&---------------------------------------------------------------------*

PROGRAM  sapmzles001   MESSAGE-ID zles.

***********************************************************************
* Tabelas transparentes
***********************************************************************
TABLES: komk,
        komp,
        zlest0008,
        zlest0018,
        zlest0020,
        zlest0022,
        zlest0025,
        zib_contabil,
        zib_contabil_chv.

***********************************************************************
* Definição de tipos
***********************************************************************
TYPE-POOLS: icon,
            vrm.

TYPES: BEGIN OF ty_203_tc_lotes.
        INCLUDE STRUCTURE zles_cockpit_lote.
TYPES:  bukrs                  TYPE bukrs,
        gjahr                  TYPE gjahr,
        acrdecr                TYPE c,
        mark,
      END    OF ty_203_tc_lotes.

TYPES: BEGIN OF ty_zlest0022.
        INCLUDE STRUCTURE zlest0022.
TYPES: deschvid	TYPE zdeschvid,
       END OF ty_zlest0022.

TYPES: BEGIN OF ty_300_tc_lacto.
        INCLUDE STRUCTURE zles_cockpit_lancto.
TYPES:  mark,
      END    OF ty_300_tc_lacto.

TYPES: BEGIN OF ty_400_tc_confer,
         codtrp                TYPE lifnr,
         dscodtrp              TYPE name1,
         codposto              TYPE lifnr,
         dscodposto            TYPE name1,
         lote	                 TYPE char10,
         chvid                 TYPE zchvid,
         deschvid              TYPE zdeschvid,
         ctlgchavid            TYPE zctlgchavid,
         status                TYPE ztatuslote,
         ctafrete              TYPE zctafrete,
         conhec                TYPE zconhec,
         data_acresc           TYPE d,
         valor_acresc          TYPE kwert,
         valor_acdc_ad         TYPE kwert,
         observacoes(255)      TYPE c,
         acrescentado,
         mark,
         check,
*        Pesos conhecimento
         peso_origem           TYPE brgew_ap,
         peso_importado        TYPE brgew_ap,
         peso_conferido	       TYPE brgew_ap,
         peso_diferenca        TYPE brgew_ap,
         peso_perda            TYPE brgew_ap,
         peso_qbr_tolerav      TYPE brgew_ap,
*        Valores descontados
         vlr_seguro            TYPE kwert,
         vlr_importado         TYPE kwert,
         vlr_programado        TYPE kwert,
         vlr_perda             TYPE kwert,
         vlr_sobra_quebra      TYPE kwert,
         vlr_conferido         TYPE kwert,
         vlr_diferenca         TYPE kwert,
         vlr_adiantamento      TYPE kwert,
         vlr_imp_retido        TYPE kwert,
         vlr_tarifa            TYPE kwert,
         dtacheg               TYPE erdat,
         docsap1               TYPE belnr_d,
         docsap2               TYPE belnr_d,
         docsap3               TYPE belnr_d,
       END   OF ty_400_tc_confer,

       BEGIN OF ty_204_tc_acrdecr,
         acao                  TYPE c,
         codtrp                TYPE lifnr,
         dscodtrp              TYPE name1,
         codposto              TYPE lifnr,
         dscodposto            TYPE name1,
         lote                  TYPE char10,
         vlrrealizado          TYPE kwert,
         vlrlancto             TYPE kwert,
       END   OF ty_204_tc_acrdecr,

       BEGIN OF ty_texto_edit,
         line                  TYPE bdc_vtext1,
       END   OF ty_texto_edit,

       BEGIN OF ty_obj_contabil,
         codtrp                TYPE zcodtrp,
         codposto              TYPE zcodposto,
         lote                  TYPE char10,
         chvid                 TYPE zchvid,
         tiptransp             TYPE shtyp,
         ctlglancto            TYPE zctlglancto,
         valor                 TYPE kwert,
         obj_key               TYPE awkey,
         docsap                TYPE belnr_d,
         conhec                TYPE zconhec,
         gjahr                 TYPE gjahr,
         bukrs                 TYPE bukrs,
         data_confer           TYPE dats,
         observ                TYPE reorxcomment,
         acdcusado             TYPE etrue,
        END   OF ty_obj_contabil,

       BEGIN OF ty_vlrctlglcto,
         ctlglancto            TYPE zctlglancto,
         tiptransp             TYPE shtyp,
         ctdebito              TYPE hkont,
         razesp_d              TYPE umskz,
         chvlcto_d             TYPE bschl,
         ctcredito             TYPE hkont,
         razesp_c              TYPE umskz,
         chvlcto_c             TYPE bschl,
         valor                 TYPE kwert,
         tipoconta_d           TYPE ztipoconta,
         tipoconta_c           TYPE ztipoconta,
         historico             TYPE qktextobj,
         zuonr_d               TYPE dzuonr,
         zuonr_c               TYPE dzuonr,
        END   OF ty_vlrctlglcto,

        BEGIN OF ty_idxacrdecr,
         index                 TYPE i,
         valor                 TYPE kwert,
        END   OF ty_idxacrdecr.

***********************************************************************
* Controles de Tela
***********************************************************************
CONTROLS  vg_main100_tabstrip   TYPE TABSTRIP.

CONTROLS: vg_sbs203_tabcontrol  TYPE TABLEVIEW USING SCREEN 203,
          vg_sbs204h_tabcontrol TYPE TABLEVIEW USING SCREEN 204,
          vg_sbs204d_tabcontrol TYPE TABLEVIEW USING SCREEN 204,
          vg_sbs300_tabcontrol  TYPE TABLEVIEW USING SCREEN 300,
          vg_sbs400_tabcontrol  TYPE TABLEVIEW USING SCREEN 400.

DATA: vg_col_tabcontrol         TYPE cxtab_column,

      vg_aut_eliminar_lote      TYPE z_auth_cockpit,
      vg_tem_autoriz            VALUE space,
      vg_indx_selec_200         TYPE i,
      vg_existe_log_200         VALUE space,
      vg_ctrl_inout_1200        VALUE space,
      vg_confirmado_0205        TYPE c LENGTH 1.

***********************************************************************
* Constantes
***********************************************************************
CONSTANTS: BEGIN OF cc_100_tabstrip,
             tab1              LIKE sy-ucomm  VALUE 'TAB_LOTES',
             tab2              LIKE sy-ucomm  VALUE 'TAB_LANCTO',
             tab3              LIKE sy-ucomm  VALUE 'TAB_CONFER',
             tab4              LIKE sy-ucomm  VALUE 'TAB_LOG',
           END   OF cc_100_tabstrip.

CONSTANTS: cc_26  TYPE c LENGTH 02 VALUE '26',
           cc_27  TYPE c LENGTH 02 VALUE '27',
           cc_28  TYPE c LENGTH 02 VALUE '28',
           cc_29  TYPE c LENGTH 02 VALUE '29',
           cc_a                                 VALUE 'A',
           cc_c                                 VALUE 'C',
           cc_d                                 VALUE 'D',
           cc_e                                 VALUE 'E',
           cc_i                                 VALUE 'I',
           cc_j                                 VALUE 'J',
           cc_s                                 VALUE 'S',
           cc_m                                 VALUE 'M',
           cc_n                                 VALUE 'N',
           cc_u                                 VALUE 'U',
           cc_x                                 VALUE 'X',
           cc_w                                 VALUE 'W',
           cc_on                                VALUE '1',
           cc_off                               VALUE '0',
           cc_liberado                          VALUE '2',
           cc_bloqueado                         VALUE '1',
           cc_importado                         VALUE 'I',
           cc_a_confirmar                       VALUE 'A',
           cc_confirmado                        VALUE 'C',
           cc_ctrl_manual                       VALUE 'M',
           cc_1_adacdc                          VALUE '1',
           cc_2_adacdc                          VALUE '2',
           cc_3_adacdc                          VALUE '3',
           cc_vbtypv_j              TYPE vbtypl    VALUE 'J',
           cc_vbtypn_m              TYPE vbtypl    VALUE 'M',
           cc_vbtypn_r              TYPE vbtypl    VALUE 'R',
           cc_reftyp_bi             TYPE j_1breftyp VALUE 'BI',
           cc_reftyp_md             TYPE j_1breftyp VALUE 'MD',
           cc_eq(2)            TYPE c           VALUE 'EQ',
           cc_chvid_adiant(2)  TYPE c           VALUE 'AD',
           cc_chvid_autpag(2)  TYPE c           VALUE 'AP',
           cc_chvid_admaior(2) TYPE c           VALUE 'AM',
           cc_chvid_admenor(2) TYPE c           VALUE 'AN',
           cc_bschl_29         TYPE bschl       VALUE '29',
           cc_bschl_39         TYPE bschl       VALUE '39',
           cc_tdoc_adto        TYPE blart       VALUE 'ME',
           cc_tdoc_conf        TYPE blart       VALUE 'SF',
           cc_ctlg_quebra      TYPE zctlglancto VALUE 'Q',
           cc_ctlg_sobra       TYPE zctlglancto VALUE 'S',
           cc_ctlg_perda       TYPE zctlglancto VALUE 'P',
           cc_ctlg_vlrconfer   TYPE zctlglancto VALUE 'VC',
           cc_ctlg_vlrprogdo   TYPE zctlglancto VALUE 'VP',
           cc_tipcta_fs        TYPE ztipoconta  VALUE 'FS',
           cc_tipcta_fp        TYPE ztipoconta  VALUE 'FP',
           cc_parvw_pv         TYPE parvw       VALUE 'PV',
           cc_kokrs_magi       TYPE kokrs       VALUE 'MAGI',
           cc_max_adiantamento TYPE kwert       VALUE '5000.00'.

***********************************************************************
* Definições de ponteiros
***********************************************************************
FIELD-SYMBOLS <docsap>         TYPE c.

***********************************************************************
* Definições de tabelas
***********************************************************************
DATA: ti_203_lotes             TYPE STANDARD TABLE OF ty_203_tc_lotes
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_204h_acrdecr          TYPE STANDARD TABLE OF ty_204_tc_acrdecr
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_204d_acrdecr          TYPE STANDARD
                                    TABLE OF zles_cockpit_acrescdecres
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_300_lacto             TYPE STANDARD TABLE OF ty_300_tc_lacto
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_400_confer            TYPE STANDARD TABLE OF ty_400_tc_confer
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_700_log               TYPE STANDARD TABLE OF ty_texto_edit,
      ti_400_obs               TYPE STANDARD TABLE OF ty_texto_edit,
      ti_obj_contabil          TYPE STANDARD TABLE OF ty_obj_contabil
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_vlrctlglcto           TYPE STANDARD TABLE OF ty_vlrctlglcto
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_idxacrdecr            TYPE STANDARD TABLE OF ty_idxacrdecr
                                    WITH HEADER LINE INITIAL SIZE 0,

      ti_cockpit_lote          TYPE zles_cockpit_lote_t,
      ti_cockpit_lancto        TYPE zles_cockpit_lancto_t,
      ti_cockpit_deltas        TYPE zles_cockpit_delta_t,
      ti_cockpit_confer        TYPE zles_cockpit_confer_t,
      ti_cockpit_acrdecr       TYPE zles_cockpit_acrescdecres_t,

      ti_docpartner            TYPE STANDARD TABLE OF j_1bnfnad
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_docitem               TYPE STANDARD TABLE OF j_1bnflin
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_docitemtax            TYPE STANDARD TABLE OF j_1bnfstx
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_docheadermsg          TYPE STANDARD TABLE OF j_1bnfftx
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_docrefermsg           TYPE STANDARD TABLE OF j_1bnfref
                                    WITH HEADER LINE INITIAL SIZE 0,
      ti_ext_item              TYPE STANDARD TABLE OF j_1binlin
                                    WITH HEADER LINE INITIAL SIZE 0,

      ti_excl_guicode          TYPE TABLE OF gui_code INITIAL SIZE 10,

      it_zlest0022             TYPE TABLE OF ty_zlest0022 WITH HEADER LINE,
      wa_zlest0022             TYPE ty_zlest0022.

***********************************************************************
* Estruturas
***********************************************************************
DATA: BEGIN OF stotais_lote_203,
         tot_vimportado        TYPE kwert,
         tot_vconfirmado       TYPE kwert,
         tot_vrecusado         TYPE kwert,
         tot_vrealizado        TYPE kwert,
         tot_vlr_acrec_desc    TYPE kwert,
         tot_vlr_a_pagar       TYPE kwert,
      END   OF stotais_lote_203,

      BEGIN OF stotais_lancto_203,
         tot_porigem           TYPE brgew_ap,
         tot_pimportado        TYPE brgew_ap,
         tot_pconfirmado       TYPE brgew_ap,
         tot_vorigem           TYPE kwert,
         tot_vimportado        TYPE kwert,
         tot_vconfirmado       TYPE kwert,
         tot_vdiferenca        TYPE kwert,
         tot_vprogramado       TYPE kwert,
      END   OF stotais_lancto_203,

      BEGIN OF sheader_300,
         codtrp                TYPE lifnr,
         dscodtrp              TYPE name1,
         codposto              TYPE lifnr,
         dscodposto            TYPE name1,
         lote                  TYPE char10,
         data_fechamento       TYPE erdat,
      END   OF sheader_300,

      BEGIN OF sdetlh_confer_400,
*        Controle
         peso_origem           TYPE brgew_ap,
         peso_conferido	       TYPE brgew_ap,
         peso_diferenca        TYPE brgew_ap,
         peso_perda            TYPE brgew_ap,
         peso_qbr_tolerav      TYPE brgew_ap,
         vlr_nf                TYPE kwert,
         vlr_origem            TYPE kwert,
         vlr_progr             TYPE kwert,
*        Impostos retidos
         seguro                TYPE kwert,
         vlr_impretido         TYPE kwert,
         vlr_perda             TYPE kwert,
         vlr_sobra_quebra      TYPE kwert,
         vlr_conferido         TYPE kwert,
         dtacheg               TYPE erdat,
*        Saldo do Histórico
         saldo_historico       TYPE kwert,
         docdetalhe(100)       TYPE c,
*        Controle de lançamentos
         lctochvid             TYPE zlctochvid,
         chvid                 TYPE zchvid,
*        Documento SAP de Lançamento contábil
         docsap1               TYPE belnr_d,
         docsap2               TYPE belnr_d,
         docsap3               TYPE belnr_d,
         docsap4               TYPE belnr_d,
         doc1msg(220)           TYPE c,
         doc2msg(220)           TYPE c,
         doc3msg(220)           TYPE c,
         doc4msg(220)           TYPE c,
*        Acréscimo e/ou Decréscimo
         vlr_acre_decre        TYPE kwert,
      END   OF sdetlh_confer_400,

      BEGIN OF snlancto_confer_400,
         chvid                 TYPE zchvid,
         data                  TYPE d,
         deschvid              TYPE zdeschvid,
         valor                 TYPE kwert,
         observ1(84)           TYPE c,
         observ2(84)           TYPE c,
         observ3(84)           TYPE c,
      END   OF snlancto_confer_400,

      BEGIN OF sctrl_saldo_400,
        saldo_atual            TYPE kwert,
        saldo_hist             TYPE kwert,
        edit_lanc,
        hist_negat,
      END   OF sctrl_saldo_400,

      BEGIN OF slog_700,
         codtrp                TYPE lifnr,
         codposto              TYPE lifnr,
         lote	                 TYPE char10,
         conhec                TYPE zconhec,
         dscodtrp              TYPE name1,
         dscodposto            TYPE name1,
      END   OF slog_700,

      BEGIN OF stela_1200,
*        range_codtrp          TYPE lxhme_range_c10_t,
*        range_codposto        TYPE lxhme_range_c10_t,
*        range_lote            TYPE lxhme_range_c10_t,
         range_conhec          TYPE lxhme_range_c10_t,
         range_periodo         TYPE lxhme_range_date_t,
         range_fechamento      TYPE lxhme_range_date_t,
         range_vencimento      TYPE lxhme_range_date_t,
         status                TYPE ztatuslote,
      END   OF stela_1200,

      BEGIN OF stela_403,
         peso_conferido        TYPE brgew_15,
         unid_peso             TYPE gewei,
         desc_unid             TYPE msehl,
         unid_alter            TYPE gewei,
         desc_ualter           TYPE msehl,
         peso_alter            TYPE brgew_15,
         idx_lancto            TYPE i,
         idx_delta             TYPE i,
      END   OF stela_403.

***********************************************************************
* Variáveis auxiliares
***********************************************************************
DATA: ok_code                  LIKE sy-ucomm,
      ok_code_205              LIKE sy-ucomm,
      ok_code_206              LIKE sy-ucomm,
      vg_save_ok               LIKE sy-ucomm,
      vg_dynnr_tabstrip        LIKE sy-dynnr,
      vg_cursor_name           TYPE fieldname,
      vg_fcode                 TYPE gui_code,
      wa_fcode                 TYPE sy-ucomm,
      it_fcode                 LIKE TABLE OF wa_fcode,
      wa_zlest0025             TYPE zlest0025.

DATA: vg_log_container_700     TYPE REF TO cl_gui_custom_container,
      vg_log_cont_name_700     TYPE scrfname VALUE 'LOG_CONTAINER',
      vg_log_edit_700          TYPE REF TO cl_gui_textedit,
      vg_log_status_700        TYPE i   VALUE 0,
      vg_log_mod_readonly_700  TYPE i,

      vg_obs_container_400     TYPE REF TO cl_gui_custom_container,
      vg_obs_cont_name_400     TYPE scrfname VALUE 'OBSERV_CONTAINER',
      vg_obs_edit_400          TYPE REF TO cl_gui_textedit,
      vg_obs_status_400        TYPE i   VALUE 0,
      vg_obs_mod_readonly_400  TYPE i,

      vg_wa_lote               TYPE zles_cockpit_lote,
      vg_wa_lancto             TYPE zles_cockpit_lancto,
      vg_wa_deltas             TYPE zles_cockpit_delta,
      vg_wa_confer             TYPE zles_cockpit_confer,
      vg_wa_acrdecr            TYPE zles_cockpit_acrescdecres,
      vg_wa_dd07v              TYPE dd07v,

      vg_wa_400_confer         TYPE ty_400_tc_confer,
      vg_wa_docheader          TYPE j_1bnfdoc,
      vg_wa_ext_header         TYPE j_1bindoc.

DATA: vg_msgerro               TYPE bapi_msg,
      vg_campo_char20          TYPE char20,
      vg_campo_char05          TYPE char05,
      vg_campo_char40          TYPE char40,
      vg_campo_sgtxt           TYPE sgtxt,
      vg_campo_curr            TYPE kwert,
      vg_kostl                 TYPE kostl,
      vg_index                 TYPE i,
      vg_dd_name               TYPE ddobjname,
      vg_dd_value              TYPE domvalue_l,
      vg_resposta.

DATA: vg_vlr_quebra            TYPE kwert,
      vg_vlr_sobra             TYPE kwert,
      vg_vlr_perda             TYPE kwert,
      vg_vlr_confer            TYPE kwert,
      vg_vlr_programado        TYPE kwert,
      vg_zfbdt                 TYPE bseg.

DATA: prim_info_lista          TYPE c LENGTH 1,
      gs_layout                TYPE lvc_s_layo,
      alv_inf_lista            TYPE REF TO cl_gui_alv_grid,
      container_inf_lista      TYPE REF TO cl_gui_custom_container,
      it_catalog_inf_lista     TYPE lvc_t_fcat,
      wa_catalog               TYPE lvc_s_fcat.


***********************************************************************
*Parametros de tela para tela Subtela-200
***********************************************************************
INCLUDE mzles001p200. "Inserir este include no fim definição Global
