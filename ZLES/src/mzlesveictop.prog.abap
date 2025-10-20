*&---------------------------------------------------------------------*
*&  Include           MZLESVEICTOP
*&---------------------------------------------------------------------*

TABLES zlest0002.

*&---------------------------------------------------------------------*
*&  Tipos de Dados
*&---------------------------------------------------------------------*
TYPES: BEGIN OF it_veiculo_tela.
         INCLUDE STRUCTURE zlest0002.
TYPES:   mark  TYPE c LENGTH 1,
         name1 TYPE name1_gp,
       END OF it_veiculo_tela.

TYPES: BEGIN OF ty_help_domicilio,
         country    TYPE land1_gp,
         region     TYPE regio,
         taxjurcode TYPE txjcd,
         text       TYPE text60,
         pstcd_from TYPE j_1bpstcdfrom,
       END OF ty_help_domicilio,

       BEGIN OF ty_help_modelo,
         marca  TYPE zlest0092-marca,
         modelo TYPE zlest0092-modelo,
       END OF ty_help_modelo.

*&---------------------------------------------------------------------*
*&  Work Areas
*&---------------------------------------------------------------------*
DATA: wa_veiculos      TYPE zlest0002,
      wa_veiculo_tela  TYPE it_veiculo_tela,
      wa_zlest0002_mem TYPE zlest0002,
      st_ret           TYPE ddshretval,
      ls_object        TYPE borident,
      wa_veiculo_placa TYPE zsdt0001.

DATA: go_myobject  TYPE REF TO cl_gos_manager.
*&---------------------------------------------------------------------*
*&  Tabelas Internas
*&---------------------------------------------------------------------*
DATA: it_veiculos     TYPE TABLE OF zlest0002 WITH HEADER LINE INITIAL SIZE 0,
      it_veiculo_tela TYPE TABLE OF it_veiculo_tela WITH HEADER LINE INITIAL SIZE 0,
      it_zlest0002    TYPE TABLE OF zlest0002 WITH HEADER LINE INITIAL SIZE 0,
      t_dynpfields    TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1 WITH HEADER LINE,
      it_cards        TYPE TABLE OF zlest0002_card WITH HEADER LINE,
      it_zlest0216    TYPE TABLE OF zlest0216,
      wa_cards        TYPE zlest0002_card,
      t_ret           TYPE TABLE OF ddshretval.

*&---------------------------------------------------------------------*
*&  Variáveis
*&---------------------------------------------------------------------*
DATA: ok_code      LIKE sy-ucomm,
      vg_modal     LIKE sy-ucomm,
      vg_2001      TYPE c LENGTH 4,
      vg_alterou   TYPE c LENGTH 1,
      vg_pesquis   TYPE c LENGTH 1,
      nome_fornec  TYPE lfa1-name1,
      bahns        TYPE lfa1-bahns,
      cnpj_forn    TYPE lfa1-stcd1,
      cpf_forn     TYPE lfa1-stcd2,
      nome_fornec2 TYPE lfa1-name1,
      bahns2       TYPE lfa1-bahns,
      cnpj_forn2   TYPE lfa1-stcd1,
      cpf_forn2    TYPE lfa1-stcd2.

*data:ZLEST0002-PROPRIET_COMODATO(10) TYPE c.
*&SPWIZARD: DECLARATION OF TABLECONTROL 'TAB_PES_VEICULO' ITSELF
CONTROLS: tab_pes_veiculo TYPE TABLEVIEW USING SCREEN 1002.
