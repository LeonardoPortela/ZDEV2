*&---------------------------------------------------------------------*
*& Report  ZLESR0088                                                  &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 19/01/2015                                              &*
*& Descrição: Relatório – Acompanhamento doc transp x MIRO            &*
*& Transação: ZLES                                                    &*
*&--------------------------------------------------------------------&*

REPORT  zlesr0088 MESSAGE-ID z_les.
TYPE-POOLS vrm.
*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*
TABLES: zlest0032, t001, lfa1, ekbe .

*=============================================================================*
*Estrutura                                                                    *
*=============================================================================*
TYPES: BEGIN OF ty_saida,
         data            TYPE zlest0032-data,
         tknum           TYPE zlest0032-tknum,
         tdlnr           TYPE vttk-tdlnr,
         add03           TYPE zlest0032-add03,
         fknum           TYPE zlest0032-fknum,
         ebeln           TYPE zlest0032-ebeln,
         lblni           TYPE zlest0032-lblni,
         tplst           TYPE vttk-tplst,
         belnr           TYPE zlest0032-belnr,
         docnum          TYPE zlest0032-docnum,
         doc_seguro      TYPE zlest0032-obj_key_seg,
         doc_pedagio     TYPE zlest0032-obj_key_ped,
         obj_key_sub     TYPE zlest0032-obj_key_sub,
         obj_key_rec     TYPE zlest0032-obj_key_rec,
         doc_seguro_ano  TYPE char04,
         doc_pedagio_ano TYPE char04,
         aux_bukrs       TYPE zib_contabil_chv-bukrs,
         bukrs           TYPE t001-bukrs,
         name1           TYPE lfa1-name1,
         netwr           TYPE vfkp-netwr,
         rmwwr           TYPE rbkp-rmwwr,
         lifnr           TYPE lfa1-lifnr,
         docnum_sub      TYPE zlest0032-docnum_sub,  "CS202200102-25.04.2024-JT-#96438-inicio
       END OF ty_saida.

*-CS2019001083 - 03.08.2021 - JT - inicio
TYPES: BEGIN OF ty_saida2,
         budat      TYPE ekbe-budat,
         belnr      TYPE ekbe-belnr,
         gjahr      TYPE ekbe-gjahr,
         dmbtr      TYPE ekbe-dmbtr,
         bukrs      TYPE ekpo-bukrs,
         ebeln      TYPE ekbe-ebeln,
         lfbnr      TYPE ekbe-lfbnr,
         docnum     TYPE zlest0032-docnum,
         nfenum     TYPE j_1bnfdoc-nfenum,
         erdat      TYPE vttk-erdat,
         tknum      TYPE vttk-tknum,
         tdlnr      TYPE vttk-tdlnr,
         name1      TYPE j_1bnfdoc-name1,
         add03      TYPE vttk-add03,
         lifnr      TYPE vtpa-lifnr,
         fknum      TYPE zlest0032-fknum,
         netwr      TYPE vfkp-netwr,
         docnum_sub TYPE zlest0032-docnum_sub,  "CS202200102-25.04.2024-JT-#96438-inicio
       END OF ty_saida2.

TYPES: BEGIN OF ty_ekbe,
         ebeln TYPE ekbe-ebeln,
         ebelp TYPE ekbe-ebelp,
         zekkn TYPE ekbe-zekkn,
         vgabe TYPE ekbe-vgabe,
         gjahr TYPE ekbe-gjahr,
         belnr TYPE ekbe-belnr,
         buzei TYPE ekbe-buzei,
         dmbtr TYPE ekbe-dmbtr,
         xblnr TYPE ekbe-xblnr,
         lfgja TYPE ekbe-lfgja,
         lfbnr TYPE ekbe-lfbnr,
         werks TYPE ekbe-werks,
         bsart TYPE ekko-bsart,
         bukrs TYPE ekko-bukrs,
         budat TYPE ekbe-budat,
         tknum TYPE vttk-tknum.
TYPES: END   OF ty_ekbe.

TYPES: BEGIN OF ty_vttk2,
         tknum TYPE vttk-tknum,
         erdat TYPE vttk-erdat,
         add03 TYPE vttk-add03,
         tdlnr TYPE vttk-tdlnr,
         name1 TYPE lfa1-name1.
TYPES: END   OF ty_vttk2.

TYPES: BEGIN OF ty_0032,
         tknum      TYPE zlest0032-tknum,
         fknum      TYPE zlest0032-fknum,
         ebeln      TYPE zlest0032-ebeln,
         ebelp      TYPE zlest0032-ebelp,
         lblni      TYPE zlest0032-lblni,
         docnum     TYPE zlest0032-docnum,
         docnum_sub TYPE zlest0032-docnum_sub.  "CS202200102-25.04.2024-JT-#96438-inicio
TYPES: END   OF ty_0032.

TYPES: BEGIN OF ty_jdoc,
         docnum TYPE j_1bnfdoc-docnum,
         nfenum TYPE j_1bnfdoc-nfenum,
         name1  TYPE j_1bnfdoc-name1.
TYPES: END   OF ty_jdoc.

TYPES: BEGIN OF ty_vtpa2,
         vbeln TYPE vtpa-vbeln,
         posnr TYPE vtpa-posnr,
         parvw TYPE vtpa-parvw,
         lifnr TYPE vtpa-lifnr.
TYPES: END   OF ty_vtpa2.

TYPES: BEGIN OF ty_vfkp2,
         fknum TYPE vfkp-fknum,
         fkpos TYPE vfkp-fkpos,
         netwr TYPE vfkp-netwr.
TYPES: END   OF ty_vfkp2.
*-CS2019001083 - 03.08.2021 - JT - fim

TYPES: BEGIN OF ty_rbkp,
         buzei TYPE rseg-buzei.
         INCLUDE STRUCTURE rbkp.
TYPES: END OF ty_rbkp.

TYPES: BEGIN OF ty_zlest0032,
         tknum       TYPE zlest0032-tknum,
         fknum       TYPE zlest0032-fknum,
         ebeln       TYPE zlest0032-ebeln,
         ebelp       TYPE zlest0032-ebelp,
         lblni       TYPE zlest0032-lblni,
         add03       TYPE zlest0032-add03,
         belnr       TYPE zlest0032-belnr,
         data        TYPE zlest0032-data,
         hora        TYPE zlest0032-hora,
         nome        TYPE zlest0032-nome,
         gjahr       TYPE zlest0032-gjahr,
         status1     TYPE zlest0032-status1,
         docnum      TYPE zlest0032-docnum,
         obj_key_seg TYPE zlest0032-obj_key_seg,
         obj_key_ped TYPE zlest0032-obj_key_ped,
         obj_key_sub TYPE zlest0032-obj_key_sub,
         obj_key_rec TYPE zlest0032-obj_key_rec,
         lifnr       TYPE vtpa-lifnr,
         parvw       TYPE vtpa-parvw,
         docnum_sub  TYPE zlest0032-docnum_sub,  "CS202200102-25.04.2024-JT-#96438-inicio
       END OF ty_zlest0032.

*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*
DATA: it_zlest0032        TYPE TABLE OF ty_zlest0032,
      it_vttk             TYPE TABLE OF vttk,
      it_vtpa             TYPE TABLE OF vtpa,
      it_ttds             TYPE TABLE OF ttds,
      it_lfa1             TYPE TABLE OF lfa1,
      it_vfkp             TYPE TABLE OF vfkp,
      it_rbkp             TYPE TABLE OF ty_rbkp,
      it_rseg             TYPE TABLE OF rseg,
      it_zib_contabil_chv TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
      it_saida            TYPE TABLE OF ty_saida,
      it_saida_aux        TYPE TABLE OF ty_saida,
*
      it_saida2           TYPE TABLE OF ty_saida2,
      it_ekbe             TYPE TABLE OF ty_ekbe,
      it_jdoc             TYPE TABLE OF ty_jdoc,
      it_vtpa2            TYPE TABLE OF ty_vtpa2,
      it_vttk2            TYPE TABLE OF ty_vttk2,
      it_vfkp2            TYPE TABLE OF ty_vfkp2,
      it_0032             TYPE TABLE OF ty_0032.

*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*
DATA: wa_zlest0032          TYPE ty_zlest0032,
      wa_vttk               TYPE vttk,
      wa_vtpa               TYPE vtpa,
      wa_ttds               TYPE ttds,
      wa_lfa1               TYPE lfa1,
      wa_vfkp               TYPE vfkp,
      wa_rbkp               TYPE ty_rbkp,
      wa_rseg               TYPE rseg,
      wa_zib_contabil_chv   TYPE zib_contabil_chv,
      wa_zib_contabil_chv_2 TYPE zib_contabil_chv,
      wa_zib_contabil_chv_3 TYPE zib_contabil_chv,
      wa_aux_data           TYPE zlest0032-data,
      wa_aux_obj_key        TYPE zib_contabil_chv-obj_key,
      wa_aux_obj_key_1      TYPE zib_contabil_chv-obj_key,
      aux_bukrs             TYPE zib_contabil_chv-bukrs,
      wa_saida              TYPE ty_saida,
      wa_saida_aux          TYPE ty_saida,
*
      wa_saida2             TYPE ty_saida2,
      wa_ekbe               TYPE ty_ekbe,
      wa_jdoc               TYPE ty_jdoc,
      wa_vtpa2              TYPE ty_vtpa2,
      wa_vttk2              TYPE ty_vttk2,
      wa_vfkp2              TYPE ty_vfkp2,
      wa_0032               TYPE ty_0032.

*=============================================================================*
*WORK AREA  TELA                                                              *
*=============================================================================*
DATA: wa_cont   TYPE REF TO cl_gui_custom_container,
      wa_alv    TYPE REF TO  cl_gui_alv_grid,
      wa_layout TYPE lvc_s_layo.

*----------------------------------------------------------------------*
***INCLUDE Zlesr0088_0001 .
*----------------------------------------------------------------------*

DATA: dg_dyndoc_id     TYPE REF TO cl_dd_document.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_xstr TYPE xstring.
DATA: graphic_size   TYPE i.
DATA: l_graphic_conv TYPE i.
DATA: l_graphic_offs TYPE i.
DATA: l_col_pos      TYPE i.

DATA: vemp TYPE t001-bukrs.
*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no.
*    METHODS TOP_OF_PAGE
*      FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
*      IMPORTING E_DYNDOC_ID.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Inclementação  -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    READ TABLE it_saida INTO wa_saida INDEX e_row_id.
    IF sy-subrc IS NOT INITIAL.
    ENDIF.
    CASE e_column_id-fieldname.
      WHEN 'DOC_SEGURO'.
        SET PARAMETER ID 'BLN' FIELD wa_saida-doc_seguro.
        SET PARAMETER ID 'GJR' FIELD wa_saida-doc_seguro_ano.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      WHEN 'DOC_PEDAGIO'.
        SET PARAMETER ID 'BLN' FIELD wa_saida-doc_pedagio.
        SET PARAMETER ID 'GJR' FIELD wa_saida-doc_pedagio_ano.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDCASE.

*-CS2019001083 - 03.08.2021 - JT - inicio
    READ TABLE it_saida2 INTO wa_saida2 INDEX e_row_id.
    IF sy-subrc IS NOT INITIAL.
    ENDIF.
    CASE e_column_id-fieldname.
      WHEN 'BELNR'.
        SET PARAMETER ID 'RBN' FIELD wa_saida2-belnr.
        SET PARAMETER ID 'GJR' FIELD wa_saida2-gjahr.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
    ENDCASE.
*-CS2019001083 - 03.08.2021 - JT - fim

  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*=============================================================================*
*Estrutura cabeçalho Alv                                                      *
*=============================================================================*
DATA: picture          TYPE REF TO cl_gui_picture,
      gf_first_display TYPE c VALUE 'X',
      ctl_cccontainer  TYPE REF TO cl_gui_custom_container,
      dg_splitter      TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_parent_html   TYPE REF TO cl_gui_container,
      dg_parent_html1  TYPE REF TO cl_gui_container,
      dg_parent_html2  TYPE REF TO cl_gui_container,
      dg_parent_grid   TYPE REF TO cl_gui_container,
      event_handler    TYPE REF TO lcl_event_handler,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      ctl_alv_resumo   TYPE REF TO cl_gui_alv_grid,
      gs_scroll_col    TYPE lvc_s_col,
      gs_scroll_row    TYPE lvc_s_roid,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      it_exclude_fcode TYPE ui_functions.


*=============================================================================*
*Estrutura Alv                                                                *
*=============================================================================*
DATA:it_fcat    TYPE TABLE OF lvc_s_fcat.
DATA:it_list    TYPE vrm_values,
     list_value TYPE vrm_values.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.



DATA: t_top        TYPE slis_t_listheader,
      xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      t_print      TYPE slis_print_alv,
      v_report     LIKE sy-repid,
      t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.

DATA: "GS_VARIANTE   TYPE DISVARIANT, " é para poder escolher o layout
  variante     LIKE disvariant,
  gs_variant_c TYPE disvariant.

DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.
*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-004.
  PARAMETERS    : p_transp RADIOBUTTON   GROUP g1 USER-COMMAND usr1 DEFAULT 'X',
                  p_miro   RADIOBUTTON   GROUP g1.
SELECTION-SCREEN: END   OF BLOCK b0.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_bukrs     FOR t001-bukrs            MODIF ID t1, "  OBLIGATORY,            "Empresa
                   s_data      FOR zlest0032-data        MODIF ID t2, "  OBLIGATORY,            "Data transporte
                   s_budat     FOR ekbe-budat            MODIF ID t3, "  OBLIGATORY,            "Data miro
                   s_belnr     FOR ekbe-belnr            MODIF ID t3, "  OBLIGATORY,            "docto miro
                   s_doc_t     FOR zlest0032-tknum       MODIF ID t2,                           "Doc_Transporte
                   s_doc_c     FOR zlest0032-fknum       MODIF ID t2,                           "Doc_Custo
                   s_miro      FOR zlest0032-belnr       MODIF ID t2,                           "Fatura
                   s_frota     FOR zlest0032-add03       MODIF ID t2 NO INTERVALS NO-EXTENSION, "Frota
                   s_propri    FOR lfa1-lifnr            MODIF ID t2,                           "Proprietario do Veiculo
                   s_doc_sg    FOR zlest0032-obj_key_seg MODIF ID t2,                           "Doc_Seguro
                   s_doc_pd    FOR zlest0032-obj_key_ped MODIF ID t2,                           "Doc_Pedágio
                   s_doc_sb    FOR zlest0032-obj_key_sub MODIF ID t2,                           "Doc_Subcontratado
                   s_doc_rc    FOR zlest0032-obj_key_rec MODIF ID t2.                           "Doc_Receita
SELECTION-SCREEN:END OF BLOCK b1.

*-CS2019001083 - 03.08.2021 - JT - inicio
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_layout        TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.
*-CS2019001083 - 03.08.2021 - JT - fim

*-CS2019001083 - 03.08.2021 - JT - inicio
*=============================================================================*
*AT SELECT SCREEN
*=============================================================================*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f_alv_variant_f4 CHANGING p_layout.

*=============================================================================*
* Tratar tela
*=============================================================================*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE abap_true.
      WHEN p_transp.
        IF screen-group1   = 'T3'.
          screen-active    = 0.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      WHEN p_miro.
        IF screen-group1   = 'T2'.
          screen-active    = 0.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.

    IF screen-name = 'S_BUKRS-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'S_DATA-LOW'.
      IF p_transp = abap_true.
        screen-required = 2.
      ELSE.
        screen-required = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'S_BUDAT-LOW'.
      IF p_miro = abap_true.
        screen-required = 2.
      ELSE.
        screen-required = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*=============================================================================*
* tratar campos obrigatorios
*=============================================================================*
AT SELECTION-SCREEN.

  CHECK sy-ucomm <> 'USR1'.

  IF      p_transp IS NOT INITIAL.
    IF s_bukrs[] IS INITIAL.
      MESSAGE s000(z01) WITH TEXT-010 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    IF s_data[] IS INITIAL.
      MESSAGE s000(z01) WITH TEXT-011 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ELSEIF  p_miro IS NOT INITIAL.
    IF s_bukrs[] IS INITIAL.
      MESSAGE s000(z01) WITH TEXT-010 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    IF s_budat[] IS INITIAL.
      MESSAGE s000(z01) WITH TEXT-012 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.
*-CS2019001083 - 03.08.2021 - JT - inicio

*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.

  gf_first_display = 'X'.

  CASE abap_true.
    WHEN p_transp.
      PERFORM: f_selecionar_dados,               " Form selecionar dado
               f_organizar_dados,                " ORGANIZAR DADOS
               definir_eventos,
               f_alv.                            "Saida ALV

    WHEN p_miro.
      PERFORM: f_selecionar_dados_miro,               " Form selecionar dado
               f_organizar_dados_miro,                " ORGANIZAR DADOS
               definir_eventos,
               f_alv_miro.                            "Saida ALV
  ENDCASE.

  IF sy-batch = 'X'. "Executar em Background
    DATA: wl_layout   TYPE slis_layout_alv.

    "WL_LAYOUT-COLWIDTH_OPTIMIZE = ABAP_TRUE.

    "GS_VARIANT-REPORT = SY-REPID.

*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        I_CALLBACK_PROGRAM = SY-REPID
*        IS_VARIANT         = GS_VARIANT
*        IT_FIELDCAT        = ESTRUTURA[]
*        IS_LAYOUT          = WL_LAYOUT
*      TABLES
*        T_OUTTAB           = IT_SAIDA
*      EXCEPTIONS
*        PROGRAM_ERROR      = 1
*        OTHERS             = 2.

    IF p_transp = abap_true.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program      = v_report
          i_callback_user_command = 'F_USER_COMMAND'
          it_fieldcat             = estrutura[]
          it_sort                 = t_sort[]
          is_layout               = wl_layout
          i_save                  = 'A'
          it_events               = events
          is_print                = t_print
        TABLES
          t_outtab                = it_saida.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program      = v_report
          i_callback_user_command = 'F_USER_COMMAND'
          it_fieldcat             = estrutura[]
          it_sort                 = t_sort[]
          is_layout               = wl_layout
          i_save                  = 'A'
          it_events               = events
          is_print                = t_print
        TABLES
          t_outtab                = it_saida2.
    ENDIF.
  ELSE.
    CALL SCREEN 0100.
  ENDIF.

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM f_selecionar_dados.

  SELECT  z~tknum,
          z~fknum,
          z~ebeln,
          z~ebelp,
          z~lblni,
          z~add03,
          z~belnr,
          z~data,
          z~hora,
          z~nome,
          z~gjahr,
          z~status1,
          z~docnum,
          z~obj_key_seg,
          z~obj_key_ped,
          z~obj_key_sub,
          z~obj_key_rec,
          v~lifnr,
          v~parvw,
          z~docnum_sub  "*-CS202200102-25.04.2024-JT-#96438
  FROM zlest0032 AS z
    INNER JOIN  vtpa AS v ON v~vbeln = z~tknum
  INTO CORRESPONDING FIELDS OF TABLE @it_zlest0032
  WHERE z~data         IN @s_data
    AND z~tknum        IN @s_doc_t
    AND z~fknum        IN @s_doc_c
    AND z~belnr        IN @s_miro
    AND z~add03        IN @s_frota
    AND z~obj_key_seg  IN @s_doc_sg
    AND z~obj_key_ped  IN @s_doc_pd
    AND z~obj_key_sub  IN @s_doc_sb
    AND z~obj_key_rec  IN @s_doc_rc
    AND v~lifnr        IN @s_propri
    AND v~parvw        EQ 'PV'.


  IF ( it_zlest0032 IS INITIAL ).
    MESSAGE s000 WITH 'Não econtrado dados Relac. frete proprio/terceiro com a revisão de fatura (Miro)' s_data s_doc_t s_doc_c .
    STOP.
  ELSE.
*
*    SELECT *
*      FROM VTPA
*      INTO TABLE IT_VTPA
*     FOR ALL ENTRIES IN IT_ZLEST0032
*      WHERE VBELN EQ IT_ZLEST0032-TKNUM
*      AND   PARVW EQ 'PV'
*      AND   LIFNR IN S_PROPRI.


    SELECT *
     FROM vttk
     INTO TABLE it_vttk
     FOR ALL ENTRIES IN it_zlest0032
     WHERE tknum EQ it_zlest0032-tknum.

    SELECT *
      FROM ttds
      INTO TABLE it_ttds
      FOR ALL ENTRIES IN it_vttk
      WHERE tplst EQ it_vttk-tplst
      AND   bukrs IN s_bukrs.


    READ TABLE it_ttds INTO wa_ttds WITH KEY bukrs = s_bukrs.
    IF sy-subrc = 0.
      vemp = wa_ttds-bukrs.
      IF vemp <> s_bukrs-low.
        MESSAGE 'Sem dados para esta empresa'   TYPE 'I'.
        STOP.
      ENDIF.
    ENDIF.


    AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
    ID 'BUKRS' FIELD s_bukrs-low
    ID 'ACTVT' FIELD '03'.

    CASE sy-subrc.
      WHEN 4.
        MESSAGE 'Sem autorização para esta empresa'   TYPE 'I'.
        STOP.
      WHEN 12.
        MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
        STOP.
    ENDCASE.

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_vttk
      WHERE lifnr EQ it_vttk-tdlnr
      AND   land1 = 'BR'.


    SELECT *
      FROM vfkp
      INTO TABLE it_vfkp
      FOR ALL ENTRIES IN it_zlest0032
      WHERE fknum EQ it_zlest0032-fknum
      AND   fkpty = 'Z001'
      AND   bukrs IN s_bukrs.

    SELECT *
      FROM rbkp AS o
      INNER JOIN rseg AS b ON b~belnr EQ o~belnr AND b~gjahr EQ o~gjahr
      INTO CORRESPONDING FIELDS OF TABLE it_rbkp
      FOR ALL ENTRIES IN it_zlest0032
      WHERE o~belnr  EQ it_zlest0032-belnr
      AND   o~gjahr  EQ it_zlest0032-gjahr
      AND   o~bukrs  IN s_bukrs.

    IF it_rbkp[] IS NOT INITIAL.
      SELECT *
        FROM rseg
        INTO TABLE it_rseg
        FOR ALL ENTRIES IN it_rbkp
        WHERE belnr EQ it_rbkp-belnr
        AND   gjahr EQ it_rbkp-gjahr.
    ENDIF.

    SELECT *
      FROM zib_contabil_chv
      INTO TABLE it_zib_contabil_chv
      FOR ALL ENTRIES IN it_zlest0032
      WHERE obj_key EQ it_zlest0032-obj_key_seg.

    SELECT *
      FROM zib_contabil_chv
     APPENDING TABLE it_zib_contabil_chv
      FOR ALL ENTRIES IN it_zlest0032
     WHERE obj_key EQ it_zlest0032-obj_key_ped.

    SORT it_zib_contabil_chv BY obj_key.

  ENDIF.
ENDFORM.                    "F_SELECIONA_DADOS

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM f_selecionar_dados_miro.

  DATA: l_tknum   TYPE numc10.

  FREE: it_ekbe, it_vttk2, it_0032,
        it_jdoc, it_vtpa2, it_vfkp2.

  SELECT ekbe~ebeln ekbe~ebelp ekbe~zekkn
         ekbe~vgabe ekbe~gjahr ekbe~belnr
         ekbe~buzei ekbe~dmbtr
         ekbe~xblnr ekbe~lfgja ekbe~lfbnr
         ekbe~werks ekko~bsart ekko~bukrs
         ekbe~budat
    FROM ekbe
   INNER JOIN ekko ON ekko~ebeln = ekbe~ebeln
    INTO TABLE it_ekbe
   WHERE ekbe~budat   IN s_budat
     AND ekbe~belnr   IN s_belnr
     AND ekbe~bewtp    = 'Q'
     AND ekbe~vgabe    = '2'
     AND ekbe~zekkn    = '01'
     AND ekko~bukrs   IN s_bukrs
     AND ekko~bsart    = 'NB'.

  IF it_ekbe[] IS INITIAL.
    MESSAGE s000 WITH 'Não foram Selecionadas informações.'.
    STOP.
  ENDIF.

  CHECK it_ekbe[] IS NOT INITIAL.

  LOOP AT it_ekbe  INTO wa_ekbe.
    l_tknum           = wa_ekbe-xblnr.
    wa_ekbe-tknum     = l_tknum.
    MODIFY it_ekbe FROM wa_ekbe INDEX sy-tabix.
  ENDLOOP.

  SELECT vttk~tknum
         vttk~erdat
         vttk~add03
         vttk~tdlnr
         lfa1~name1
    FROM vttk
   INNER JOIN lfa1 ON lfa1~lifnr = vttk~tdlnr
    INTO TABLE it_vttk2
     FOR ALL ENTRIES IN it_ekbe
   WHERE tknum = it_ekbe-tknum.

  SELECT tknum
         fknum
         ebeln
         ebelp
         lblni
         docnum
         docnum_sub  "*-CS202200102-25.04.2024-JT-#96438
    FROM zlest0032
    INTO TABLE it_0032
     FOR ALL ENTRIES IN it_ekbe
   WHERE tknum = it_ekbe-tknum
     AND ebeln = it_ekbe-ebeln
     AND lblni = it_ekbe-lfbnr.

  DELETE it_0032 WHERE docnum IS INITIAL.

  IF it_0032[] IS NOT INITIAL.
    SELECT docnum
           nfenum
           name1
      FROM j_1bnfdoc
      INTO TABLE it_jdoc
       FOR ALL ENTRIES IN it_0032
     WHERE docnum = it_0032-docnum.

    SELECT fknum
           fkpos
           netwr
      FROM vfkp
      INTO TABLE it_vfkp2
       FOR ALL ENTRIES IN it_0032
     WHERE fknum = it_0032-fknum
       AND fkpty = 'Z001'.
  ENDIF.

  IF it_vttk2[] IS NOT INITIAL.
    SELECT vbeln
           posnr
           parvw
           lifnr
      FROM vtpa
      INTO TABLE it_vtpa2
       FOR ALL ENTRIES IN it_vttk2
     WHERE vbeln = it_vttk2-tknum
       AND parvw = 'PV'.
  ENDIF.

  SORT it_vttk2 BY tknum.
  DELETE ADJACENT DUPLICATES FROM it_vttk2
                        COMPARING tknum.

  SORT it_0032 BY tknum ebeln lblni.
  DELETE ADJACENT DUPLICATES FROM it_0032
                        COMPARING tknum ebeln lblni.

  SORT it_jdoc BY docnum.

  SORT it_vfkp2 BY fknum.
  DELETE ADJACENT DUPLICATES FROM it_vfkp2
                        COMPARING fknum.

  SORT it_vtpa2 BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_vtpa2
                        COMPARING vbeln.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS                                        *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text                                                 *
*  <--  p2        text                                                 *
*----------------------------------------------------------------------*
FORM f_organizar_dados.
  DATA: doc_seguro    TYPE zlest0032-obj_key_seg,
        doc_pedagio   TYPE zlest0032-obj_key_ped,
        aux_bukrs     TYPE zib_contabil-bukrs,
        vg_sy_tabix   TYPE sytabix,

        aux_data      TYPE zlest0032-data,
        aux_obj_key   TYPE zib_contabil_chv-obj_key,
        aux_obj_key_1 TYPE zib_contabil_chv-obj_key,

        aux_emp       TYPE t001-bukrs,

        vl_miro       TYPE rbkp-rmwwr,
        vl_imp        TYPE rbkp-rmwwr,
        imp_miro      TYPE rbkp-wmwst1,
        vl_liq_miro_t TYPE rseg-wrbtr,
        vl_liq_miro   TYPE rseg-wrbtr,
        vl_bruto      TYPE rbkp-rmwwr.


  "SORT IT_ZLEST0032 BY TKNUM DESCENDING.
  LOOP AT it_zlest0032 INTO wa_zlest0032.

    CLEAR: wa_saida.

    vl_miro       = 0.
    imp_miro      = 0.
    vl_liq_miro_t = 0.
    vl_liq_miro   = 0.
    vl_bruto      = 0.


    "    READ TABLE IT_VTPA INTO WA_VTPA WITH KEY VBELN = WA_ZLEST0032-TKNUM.
    "    IF SY-SUBRC  IS INITIAL.
    wa_saida-lifnr = wa_zlest0032-lifnr.
    "    ENDIF.


    READ TABLE it_vttk INTO wa_vttk WITH KEY tknum = wa_zlest0032-tknum.
    IF sy-subrc IS INITIAL.
      wa_saida-tplst = wa_vttk-tplst.
      wa_saida-tdlnr = wa_vttk-tdlnr.
    ENDIF.


    READ TABLE it_ttds INTO wa_ttds WITH KEY tplst = wa_vttk-tplst.
    IF sy-subrc IS INITIAL.
      wa_saida-bukrs = wa_ttds-bukrs.
      aux_emp        = wa_ttds-bukrs.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vttk-tdlnr.
    IF sy-subrc IS INITIAL.
      wa_saida-name1  = wa_lfa1-name1.
    ENDIF.


    READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_zlest0032-obj_key_seg BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_saida-doc_seguro     = wa_zib_contabil_chv-belnr.
      wa_saida-doc_seguro_ano = wa_zib_contabil_chv-obj_key+13(4).
      wa_saida-aux_bukrs      = wa_zib_contabil_chv-bukrs.
    ENDIF.

    READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_zlest0032-obj_key_ped BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_saida-doc_pedagio     = wa_zib_contabil_chv-belnr.
      wa_saida-doc_pedagio_ano = wa_zib_contabil_chv-obj_key+13(4).
      wa_saida-aux_bukrs       = wa_zib_contabil_chv-bukrs.
    ENDIF.


    wa_saida-data   = wa_zlest0032-data.
    wa_saida-tknum  = wa_zlest0032-tknum.
    wa_saida-add03  = wa_zlest0032-add03.
    wa_saida-fknum  = wa_zlest0032-fknum.
    wa_saida-ebeln  = wa_zlest0032-ebeln.
    wa_saida-belnr  = wa_zlest0032-belnr.
    wa_saida-lblni  = wa_zlest0032-lblni.
    wa_saida-docnum = wa_zlest0032-docnum.
    wa_saida-obj_key_sub = wa_zlest0032-obj_key_sub.
    wa_saida-obj_key_rec = wa_zlest0032-obj_key_rec.
    wa_saida-docnum_sub  = wa_zlest0032-docnum_sub.  "*-CS202200102-25.04.2024-JT-#96438

    READ TABLE it_vfkp INTO wa_vfkp WITH KEY fknum = wa_zlest0032-fknum.
    IF sy-subrc IS INITIAL.
      wa_saida-netwr = wa_vfkp-netwr.
    ENDIF.


    READ TABLE it_rbkp INTO wa_rbkp WITH KEY belnr  = wa_zlest0032-belnr
                                             gjahr  = wa_zlest0032-gjahr.
    IF sy-subrc IS INITIAL.
      vl_miro  = wa_rbkp-rmwwr.
      imp_miro = wa_rbkp-wmwst1.
    ENDIF.

    LOOP AT it_rseg INTO wa_rseg WHERE belnr = wa_rbkp-belnr. "AND
      "XBLNR = WA_ZLEST0032-TKNUM.
      vl_liq_miro_t =  vl_liq_miro + wa_rseg-wrbtr.

      CLEAR wa_rseg.
    ENDLOOP.

    READ TABLE it_rseg INTO wa_rseg WITH KEY belnr = wa_rbkp-belnr
                                             xblnr = wa_zlest0032-tknum.
    IF sy-subrc = 0.
      vl_liq_miro = wa_rseg-wrbtr.
    ENDIF.

    IF imp_miro = 0.
      IF vl_liq_miro = 0.
        vl_bruto = vl_liq_miro_t.
      ELSE.
        vl_bruto = vl_liq_miro.
      ENDIF.

      wa_saida-rmwwr = vl_bruto.
    ELSE.
      IF vl_liq_miro = 0.
        vl_imp = ( imp_miro /  vl_liq_miro_t ) * vl_liq_miro_t.
        vl_bruto = vl_liq_miro_t + vl_imp.
        wa_saida-rmwwr = vl_bruto.
      ELSE.
        vl_imp = ( imp_miro /  vl_liq_miro_t ) * vl_liq_miro.
        vl_bruto = vl_liq_miro + vl_imp.
        wa_saida-rmwwr = vl_bruto.
      ENDIF.
    ENDIF.


    IF aux_emp IN s_bukrs.
      APPEND wa_saida TO it_saida.
    ELSE.
      CONTINUE.
    ENDIF.

    CLEAR: wa_ttds,
           wa_vttk,
           wa_zlest0032,
           wa_rbkp,
           wa_lfa1,
           aux_emp,
           wa_rseg.
  ENDLOOP.

*  IF S_PROPRI IS NOT INITIAL.
*    MOVE-CORRESPONDING IT_SAIDA TO IT_SAIDA_AUX.
*    REFRESH IT_SAIDA.
*
*    LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX
*      WHERE LIFNR IN S_PROPRI.
*
*      APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
*      CLEAR WA_SAIDA_AUX.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    "F_ORGANIZAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS                                        *
*&---------------------------------------------------------------------*
FORM f_organizar_dados_miro.

  FREE: it_saida2.

  LOOP AT it_ekbe INTO wa_ekbe.

    CLEAR: wa_vttk2,
           wa_0032,
           wa_jdoc,
           wa_vfkp2,
           wa_vtpa2.

    READ TABLE it_vttk2 INTO wa_vttk2 WITH KEY tknum = wa_ekbe-tknum
                                      BINARY SEARCH.
    CHECK sy-subrc = 0.

    READ TABLE it_0032  INTO wa_0032  WITH KEY tknum = wa_ekbe-tknum
                                               ebeln = wa_ekbe-ebeln
                                               lblni = wa_ekbe-lfbnr
                                      BINARY SEARCH.

    READ TABLE it_jdoc  INTO wa_jdoc  WITH KEY docnum = wa_0032-docnum
                                      BINARY SEARCH.

    READ TABLE it_vfkp2 INTO wa_vfkp2 WITH KEY fknum = wa_0032-fknum
                                      BINARY SEARCH.

    READ TABLE it_vtpa2 INTO wa_vtpa2 WITH KEY vbeln = wa_vttk2-tknum
                                      BINARY SEARCH.

    MOVE-CORRESPONDING wa_0032        TO wa_saida2.
    MOVE-CORRESPONDING wa_jdoc        TO wa_saida2.
    MOVE-CORRESPONDING wa_vfkp2       TO wa_saida2.
    MOVE-CORRESPONDING wa_vtpa2       TO wa_saida2.
    MOVE-CORRESPONDING wa_vttk2       TO wa_saida2.
    MOVE-CORRESPONDING wa_ekbe        TO wa_saida2.

    APPEND wa_saida2                  TO it_saida2.
  ENDLOOP.

ENDFORM.

*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM f_alv.

  FREE: l_col_pos.

  IF sy-batch = 'X'.

    PERFORM alv_preenche_cat02 USING:

          'DATA'         'Data Transporte'           '10'  ''  ''   '' ,
          'TKNUM'        'Doc Transporte'            '13'  ''  ''   '' ,
          'TDLNR'        'Agente de Frete'           '13'  ''  ''   '' ,
          'ADD03'        'Tp.Fatura'                 '13'  ''  ''   '' ,
          'LIFNR'        'Prop.Veiculo'              '10'  ''  ''   '' ,
          'FKNUM'        'Doc. Custo'                '10'  ''  ''   '' ,
          'EBELN'        'Pedido'                    '13'  ''  ''   '' ,
          'LBLNI'        'Folha Serviço'             '13'  ''  ''   '' ,
          'TPLST'        'Org.Transporte'            '13'  ''  ''   '' ,
          'BELNR'        'Miro'                      '11'  ''  ''   '' ,
          'DOCNUM'       'Nº Documento'              '13'  ''  ''   '' ,
          'DOCNUM_SUB'   'Nº Doc.Subcontr'           '16'  ''  ''   '' , "*-CS202200102-25.04.2024-JT-#96438
          'DOC_SEGURO'   'Doc Seguro'                '13'  'X' ''   '' ,
          'DOC_PEDAGIO'  'Doc Pedágio'               '13'  'X' ''   '' ,
          'OBJ_KEY_SUB'  'Doc Subcontratado'         '17'  ''  ''   '' ,
          'OBJ_KEY_REC'  'Doc Receita'               '17'  ''  ''   '' ,
          'BUKRS'        'Empresa'                   ' 7'  ''  ''   '' ,
          'NAME1'        'Nome Agente Frete'         '35'  ''  ''   '' ,
          'NETWR'        'Valor VI'                  ' 8'  ''  ''   '' ,
          'RMWWR'        'Valor MIRO'                '10'  ''  ''   '' .

  ELSE.

    PERFORM alv_preenche_cat USING:

          'DATA'         'Data Transporte'           '10'  ''  ''   '' ,
          'TKNUM'        'Doc Transporte'            '13'  ''  ''   '' ,
          'TDLNR'        'Agente de Frete'           '13'  ''  ''   '' ,
          'ADD03'        'Tp.Fatura'                 '13'  ''  ''   '' ,
          'LIFNR'        'Prop.Veiculo'              '10'  ''  ''   '' ,
          'FKNUM'        'Doc Custo'                 '10'  ''  ''   '' ,
          'EBELN'        'Pedido'                    '13'  ''  ''   '' ,
          'LBLNI'        'Folha Serviço'             '13'  ''  ''   '' ,
          'TPLST'        'Org.Transporte'            '13'  ''  ''   '' ,
          'BELNR'        'Miro'                      '11'  ''  ''   '' ,
          'DOCNUM'       'Nº Documento'              '13'  ''  ''   '' ,
          'DOCNUM_SUB'   'Nº Doc.Subcontr'           '16'  ''  ''   '' , "*-CS202200102-25.04.2024-JT-#96438
          'DOC_SEGURO'   'Doc Seguro'                '13'  'X' ''   '' ,
          'DOC_PEDAGIO'  'Doc Pedágio'               '13'  'X' ''   '' ,
          'OBJ_KEY_SUB'  'Doc Subcontratado'         '17'  ''  ''   '' ,
          'OBJ_KEY_REC'  'Doc Receita'               '17'  ''  ''   '' ,
          'BUKRS'        'Empresa'                   ' 7'  ''  ''   '' ,
          'NAME1'        'Nome Agente Frete'         '35'  ''  ''   '' ,
          'NETWR'        'Valor VI'                  ' 8'  ''  ''   '' ,
          'RMWWR'        'Valor MIRO'                '10'  ''  ''   '' .


  ENDIF.

* CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
*      EXPORTING
*        IS_LAYOUT                     = GS_LAYOUT
*        I_SAVE                        = 'A'
*        IS_VARIANT                    = VARIANTE
*"       IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE_FCODE
*      CHANGING
*        IT_FIELDCATALOG               = IT_FCAT
*        IT_OUTTAB                     = IT_SAIDA
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4.

ENDFORM.    "F_ALV

*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM f_alv_miro.

  FREE: l_col_pos.

  IF sy-batch = 'X'.
    PERFORM alv_preenche_cat02 USING:
          'BUDAT'        'Dt.Miro'                   '10'  ''  ''   '' ,
          'BELNR'        'Nr.Miro'                   '15'  'X'  ''   '' ,
          'DMBTR'        'Vlr.Miro'                  '15'  ''  ''   '' ,
          'BUKRS'        'Empresa'                   '10'  ''  ''   '' ,
          'EBELN'        'Pedido'                    '12'  ''  ''   '' ,
          'LFBNR'        'Folha Serviço'             '13'  ''  ''   '' ,
          'DOCNUM'       'Docnum Miro'               '12'  ''  ''   '' ,
          'DOCNUM_SUB'   'Nº Doc.Subcontr'           '16'  ''  ''   '' , "*-CS202200102-25.04.2024-JT-#96438
          'NFENUM'       'Nr.CTE'                    '12'  ''  ''   '' ,
          'ERDAT'        'Dt.Transp.'                '10'  ''  ''   '' ,
          'TKNUM'        'Doc.Transp.'               '12'  ''  ''   '' ,
          'TDLNR'        'Agente Frete'              '13'  ''  ''   '' ,
          'NAME1'        'Nome Agente Frete'         '40'  ''  ''   '' ,
          'ADD03'        'Tipo Fatura'               '10'  ''  ''   '' ,
          'LIFNR'        'Prop.Veículo'              '10'  ''  ''   '' ,
          'FKNUM'        'Doc.Custo'                 '10'  ''  ''   '' ,
          'NETWR'        'Valor VI'                  '15'  ''  ''   '' .
  ELSE.
    PERFORM alv_preenche_cat USING:
          'BUDAT'        'Dt.Miro'                   '10'  ''  ''   '' ,
          'BELNR'        'Nr.Miro'                   '15'  'X'  ''   '' ,
          'DMBTR'        'Vlr.Miro'                  '15'  ''  ''   '' ,
          'BUKRS'        'Empresa'                   '10'  ''  ''   '' ,
          'EBELN'        'Pedido'                    '12'  ''  ''   '' ,
          'LFBNR'        'Folha Serviço'             '13'  ''  ''   '' ,
          'DOCNUM'       'Docnum Miro'               '12'  ''  ''   '' ,
          'DOCNUM_SUB'   'Nº Doc.Subcontr'           '16'  ''  ''   '' , "*-CS202200102-25.04.2024-JT-#96438
          'NFENUM'       'Nr.CTE'                    '12'  ''  ''   '' ,
          'ERDAT'        'Dt.Transp.'                '10'  ''  ''   '' ,
          'TKNUM'        'Doc.Transp.'               '12'  ''  ''   '' ,
          'TDLNR'        'Agente Frete'              '13'  ''  ''   '' ,
          'NAME1'        'Nome Agente Frete'         '40'  ''  ''   '' ,
          'ADD03'        'Tipo Fatura'               '10'  ''  ''   '' ,
          'LIFNR'        'Prop.Veículo'              '10'  ''  ''   '' ,
          'FKNUM'        'Doc.Custo'                 '10'  ''  ''   '' ,
          'NETWR'        'Valor VI'                  '15'  ''  ''   '' .
  ENDIF.

ENDFORM.

"&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat   USING   p_campo  TYPE c
                                p_desc   TYPE c
                                p_tam    TYPE c
                                p_hot    TYPE c
                                p_zero   TYPE c
                                p_sum    TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  l_col_pos = l_col_pos + 1.

  wl_fcat-col_pos   = l_col_pos.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-outputlen = p_tam.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT

"&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM container_html .

  DATA : dl_length        TYPE i,                           " Length
         dl_background_id TYPE sdydo_key VALUE space. " Background_id

  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->merge_document.

  CALL METHOD dg_dyndoc_id->set_document_background
    EXPORTING
      picture_id = dl_background_id.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html1
    EXCEPTIONS
      html_display_error = 1.

ENDFORM.                    " CONTAINER_HTML

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM add_text USING p_text  TYPE sdydo_text_element
                    p_style TYPE sdydo_attribute
                    p_size  TYPE sdydo_attribute
                    p_color TYPE sdydo_attribute.

* Adding text
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text          = p_text
      sap_style     = p_style
      sap_fontsize  = p_size
      sap_color     = p_color
      sap_fontstyle = cl_dd_area=>sans_serif.

  "SAP_STYLE    = CL_DD_AREA=>HEADING
  "SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
  "SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
ENDFORM.                    " ADD_TEXT


*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_objects OUTPUT.

  DATA: url(255) TYPE c.

* Create container and ALV objects only once
  IF gf_first_display = 'X'.

*   Create object for container
    CREATE OBJECT ctl_cccontainer
      EXPORTING
        container_name = 'TELA_0100'.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = ctl_cccontainer
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_html
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html1.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_html2.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_html2.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_grid.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 15.


*   Create object for ALV grid inside container
    CREATE OBJECT ctl_alv_resumo
      EXPORTING
        i_parent = dg_parent_grid.

*   Fill info for layout variant
    PERFORM fill_gs_variant.

    "GS_LAYOUT-SEL_MODE = 'A'.
    gs_layout-zebra      = 'X'.
    "GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

*-CS2019001083 - 03.08.2021 - JT - inicio
    gs_variant-report   = sy-repid.
    gs_variant-username = sy-uname.
    gs_variant-variant  = p_layout.
    IF p_transp = abap_true.
      gs_variant-handle = '0100'.
    ELSE.
      gs_variant-handle = '0200'.
    ENDIF.
*-CS2019001083 - 03.08.2021 - JT - fim

*   Create Object for Event Handler
    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv_resumo.
*    SET HANDLER EVENT_HANDLER->TOP_OF_PAGE          FOR CTL_ALV_RESUMO.
    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = dg_parent_grid
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

**   Send data to ALV grid
*-CS2019001083 - 03.08.2021 - JT - inicio
    IF p_transp = abap_true.
      CALL METHOD ctl_alv_resumo->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          is_variant           = gs_variant
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida.
    ELSE.
      CALL METHOD ctl_alv_resumo->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          is_variant           = gs_variant
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida2.
    ENDIF.
*-CS2019001083 - 03.08.2021 - JT - fim

    PERFORM cria_html_cab.

    CALL METHOD ctl_alv_resumo->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = dg_dyndoc_id.

    CLEAR: gf_first_display.



  ENDIF.

  CALL METHOD ctl_alv_resumo->refresh_table_display.

  CALL METHOD ctl_alv_resumo->set_scroll_info_via_id
    EXPORTING
      is_col_info = gs_scroll_col
      is_row_no   = gs_scroll_row.


ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_html_cab .

  DATA: column                  TYPE REF TO cl_dd_area,
        column_1                TYPE REF TO cl_dd_area,
        column_2                TYPE REF TO cl_dd_area,
        table_element           TYPE REF TO cl_dd_table_element,
        table_element2          TYPE REF TO cl_dd_table_element,
        p_text                  TYPE sdydo_text_element,
        p_text_table            TYPE sdydo_text_table,
        sdydo_text_element(255),
        vg_mes(2), vg_ano(4),
        qtd                     TYPE i.

  CALL METHOD dg_dyndoc_id->initialize_document.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 1
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element.

  CALL METHOD table_element->add_column
    IMPORTING
      column = column.

  CALL METHOD table_element->set_column_style
    EXPORTING
      col_no    = 1
      sap_align = 'CENTER'
      sap_style = cl_dd_document=>heading.

  p_text = 'Relatório Acompanhamento doc transp x miro'.
  CALL METHOD column->add_text
    EXPORTING
      text      = p_text
      sap_style = 'HEADING'.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element2.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_1.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_2.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 1
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 2
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  IF p_transp = abap_true.
    sdydo_text_element = 'Data:'.
    APPEND sdydo_text_element TO p_text_table.

    sdydo_text_element = 'Doc.Transp : '.
    APPEND sdydo_text_element TO p_text_table.

    sdydo_text_element = 'Doc.Custo: '.
    APPEND sdydo_text_element TO p_text_table.

    sdydo_text_element = 'Frota: '.
    APPEND sdydo_text_element TO p_text_table.
  ELSE.
    sdydo_text_element = 'Empresa:'.
    APPEND sdydo_text_element TO p_text_table.
    sdydo_text_element = 'Data Miro:'.
    APPEND sdydo_text_element TO p_text_table.
    sdydo_text_element = 'Documento Miro:'.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

  CALL METHOD column_1->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  CLEAR: p_text_table, sdydo_text_element.

  "********
  "Data *****
  IF p_transp = abap_true.
    IF s_data-low IS NOT INITIAL.
      CONCATENATE s_data-low+6(2) '.' s_data-low+4(2) '.'  s_data-low(4) INTO sdydo_text_element.
    ENDIF.
    IF s_data-high IS NOT INITIAL.
      CONCATENATE sdydo_text_element ' - '  INTO sdydo_text_element SEPARATED BY space.
      CONCATENATE sdydo_text_element '  ' s_data-high+6(2) '.' s_data-high+4(2) '.'  s_data-high(4) INTO sdydo_text_element.
    ENDIF.

    APPEND sdydo_text_element TO p_text_table.

    "Documento Transporte *********
    sdydo_text_element = s_doc_t-low+4(6).
    IF s_doc_t-high IS NOT INITIAL.
      CONCATENATE sdydo_text_element ' - '  INTO sdydo_text_element SEPARATED BY space.
      CONCATENATE sdydo_text_element '  ' s_doc_t-high+4(6) INTO sdydo_text_element.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.

    "Documento Custo *********
    sdydo_text_element = s_doc_c-low.
    IF s_doc_c-high IS NOT INITIAL.
      CONCATENATE sdydo_text_element ' - ' INTO sdydo_text_element SEPARATED BY space.
      CONCATENATE sdydo_text_element s_doc_c-high INTO sdydo_text_element.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.

    "Documento Frota *********
    sdydo_text_element = s_frota-low.
    APPEND sdydo_text_element TO p_text_table.

  ELSE.

    "Empresa
    sdydo_text_element = s_bukrs-low.
    IF s_bukrs-high IS NOT INITIAL.
      CONCATENATE sdydo_text_element ' - ' INTO sdydo_text_element SEPARATED BY space.
      CONCATENATE sdydo_text_element s_bukrs-high INTO sdydo_text_element.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.

    "Data Miro
    IF s_budat-low IS NOT INITIAL.
      CONCATENATE s_budat-low+6(2) '.' s_budat-low+4(2) '.'  s_budat-low(4) INTO sdydo_text_element.
    ENDIF.
    IF s_budat-high IS NOT INITIAL.
      CONCATENATE sdydo_text_element ' - '  INTO sdydo_text_element SEPARATED BY space.
      CONCATENATE sdydo_text_element '  ' s_budat-high+6(2) '.' s_budat-high+4(2) '.'  s_budat-high(4) INTO sdydo_text_element.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.

    "Documento Miro
    sdydo_text_element = s_belnr-low.
    IF s_belnr-high IS NOT INITIAL.
      CONCATENATE sdydo_text_element ' - ' INTO sdydo_text_element SEPARATED BY space.
      CONCATENATE sdydo_text_element s_belnr-high INTO sdydo_text_element.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.

  ENDIF.

  CALL METHOD column_2->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  PERFORM container_html.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM


FORM xtop_of_page.                                          "#EC CALLED
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos .
  PERFORM f_carregar_eventos USING: slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.

FORM f_carregar_eventos USING name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.

FORM alv_preenche_cat02   USING  p_campo  TYPE c
                                 p_desc   TYPE c
                                 p_tam    TYPE c
                                 p_hot    TYPE c
                                 p_zero   TYPE c
                                 p_sum    TYPE c.

  l_col_pos = l_col_pos + 1.

  wa_estrutura-col_pos       = l_col_pos.
  wa_estrutura-fieldname     = p_campo.
  "WA_ESTRUTURA-SELTEXT_S     = P_DESC.
  wa_estrutura-seltext_m     = p_desc.
  wa_estrutura-seltext_l     = p_desc.
  wa_estrutura-hotspot       = p_hot.
  wa_estrutura-no_zero       = p_zero.
  wa_estrutura-no_sum        = p_sum.
  wa_estrutura-outputlen     = p_tam.

  APPEND wa_estrutura TO estrutura.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
FORM f_alv_variant_f4 CHANGING pa_vari.

  DATA: rs_variant LIKE disvariant.

  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  IF p_transp = abap_true.
    rs_variant-handle = '0100'.
  ELSE.
    rs_variant-handle = '0200'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc = 0.
    pa_vari = rs_variant-variant.
  ENDIF.

ENDFORM.                               " ALV_VARIANT_F4

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
