*&---------------------------------------------------------------------*
*&  Include           ZMMR175_TOP
*&---------------------------------------------------------------------*

"Tipos
TYPES: BEGIN OF ty_bsak,
         bukrs TYPE bsak-bukrs,
         gjahr TYPE bsak-gjahr,
         belnr TYPE bsak-belnr,
         augbl TYPE bsak-augbl,
         dmbtr TYPE bsak-dmbtr,
         xblnr TYPE bsak-xblnr,
         ebeln TYPE bsak-ebeln,
         ebelp TYPE bsak-ebelp,
         augdt TYPE bsak-augdt,
         bsart TYPE ekko-bsart,
       END OF ty_bsak,

       BEGIN OF ty_screen,
         bukrs TYPE bukrs,
         augdt TYPE augdt,
         blart TYPE bsak-blart,
         gjahr TYPE bsak-gjahr,
         vgabe TYPE vgabe,
         budat TYPE budat,
         fat   TYPE c LENGTH 3000,
       END OF ty_screen,

       BEGIN OF ty_ekbe,
         belnr TYPE ekbe-belnr,
         gjahr TYPE ekbe-gjahr,
         budat TYPE ekbe-budat,
         dmbtr TYPE ekbe-dmbtr,
         ebeln TYPE ekbe-ebeln,
         ebelp TYPE ekbe-ebelp,
         bsart TYPE ekko-bsart,
         bukrs TYPE ekko-bukrs,
       END OF ty_ekbe,

       BEGIN OF ty_saida,
         tipo      TYPE char30,
         id        TYPE char20,
         bukrs     TYPE bsak-bukrs,
         gjahr     TYPE bsak-gjahr,
         belnr     TYPE bsak-belnr,
         augbl     TYPE bsak-augbl,
         dmbtr     TYPE bsak-dmbtr,
         xblnr     TYPE bsak-xblnr,
         ebeln     TYPE bsak-ebeln,
         ebelp     TYPE bsak-ebelp,
         augdt     TYPE bsak-augdt,
         bsart     TYPE ekko-bsart,
         historico TYPE string,
       END OF ty_saida.

"Tabelas
DATA: git_bsak               TYPE TABLE OF ty_bsak,
      git_zintegrcoupa01     TYPE TABLE OF zintegrcoupa01,
      git_zintegrcoupa01_aux TYPE TABLE OF zintegrcoupa01,
      git_ekbe               TYPE TABLE OF ty_ekbe,
      git_set                TYPE TABLE OF rgsb4,
      git_filter             TYPE zif_integracao_coupa_ped_comp=>tt_filter,
      git_xml_header         TYPE zcl_integracao_coupa_ped_comp=>tt_xml_header,
      git_saida              TYPE TABLE OF ty_saida.

"Range
DATA: gra_bsart     TYPE RANGE OF bsart,
      gra_bukrs     TYPE RANGE OF bukrs,
      gra_id_integr TYPE RANGE OF zintegrcoupa01-id_integr.

"Estruturas
DATA: gwa_screen     TYPE ty_screen,
      gwa_bsart      LIKE LINE OF gra_bsart,
      gwa_bukrs      LIKE LINE OF gra_bukrs,
      gwa_xml_header LIKE LINE OF git_xml_header,
      gwa_id_integr  LIKE LINE OF gra_id_integr..

"Variaveis
DATA: gva_id TYPE string.

"Objetos
DATA: go_int_ped TYPE REF TO zcl_integracao_coupa_ped_comp,
      go_table   TYPE REF TO cl_salv_table..
