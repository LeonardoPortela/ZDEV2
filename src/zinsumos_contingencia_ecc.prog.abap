* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZINSUMOS_CONTINGENCIA_ECC                               *
* Title.......: Programa que carrega dados ECC para S4 - Contingencia  *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 22/09/2023                                             *
* -------------------------------------------------------------------- *
REPORT zinsumos_contingencia_ecc.

TYPE-POOLS: slis, abap, icon.

TABLES: zsde_cont_insumos, mara, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDE_CONT_INSUMOS'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zsde_cont_insumos.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.

" 01 - ZSDT0100
DATA gt_zsdt0116_ecc TYPE TABLE OF zsdt0116.
DATA gt_zsdt0116_s4 TYPE TABLE OF zsdt0116.

" 03 - ZSDT0079
DATA gt_zsdt0082_s4 TYPE TABLE OF zsdt0082.
DATA gt_zsdt0082_ecc TYPE TABLE OF zsdt0082.
DATA gt_zsdt0082_s4_check TYPE TABLE OF zsdt0082.
DATA gt_zsdt0082_aux TYPE TABLE OF zsdt0082.

" 04 - ZSDT0081
DATA gt_zsdt0150_s4 TYPE TABLE OF zsdt0150.
DATA gt_zsdt0150_ecc TYPE TABLE OF zsdt0150.

DATA gt_zsdt0136_s4 TYPE TABLE OF zsdt0136.
DATA gt_zsdt0136_ecc TYPE TABLE OF zsdt0136.

DATA gt_zsdt0154_s4 TYPE TABLE OF zsdt0154.
DATA gt_zsdt0154_ecc TYPE TABLE OF zsdt0154.

" 06 - ZSDT0112
DATA gt_zsdt0129_s4 TYPE TABLE OF zsdt0129.
DATA gt_zsdt0129_ecc TYPE TABLE OF zsdt0129.
DATA gt_zsdt0129_aux TYPE TABLE OF zsdt0129.

DATA gt_zsdt0130_s4 TYPE TABLE OF zsdt0130.
DATA gt_zsdt0130_ecc TYPE TABLE OF zsdt0130.

DATA gt_zsdt0131_s4 TYPE TABLE OF zsdt0131.
DATA gt_zsdt0131_ecc TYPE TABLE OF zsdt0131.
DATA gt_zsdt0131_aux TYPE TABLE OF zsdt0131.

DATA gt_zsdt0133_s4 TYPE TABLE OF zsdt0133.
DATA gt_zsdt0133_ecc TYPE TABLE OF zsdt0133.
DATA gt_zsdt0133_aux TYPE TABLE OF zsdt0133.

DATA gt_zsdt0134_s4 TYPE TABLE OF zsdt0134.
DATA gt_zsdt0134_ecc TYPE TABLE OF zsdt0134.
DATA gt_zsdt0134_aux TYPE TABLE OF zsdt0134.

DATA gt_zsdt0137_s4 TYPE TABLE OF zsdt0137.
DATA gt_zsdt0137_ecc TYPE TABLE OF zsdt0137.
DATA gt_zsdt0137_aux TYPE TABLE OF zsdt0137.

DATA gt_zsdt0138_s4 TYPE TABLE OF zsdt0138.
DATA gt_zsdt0138_ecc TYPE TABLE OF zsdt0138.
DATA gt_zsdt0138_aux TYPE TABLE OF zsdt0138.

DATA gt_zsdt0139_s4 TYPE TABLE OF zsdt0139.
DATA gt_zsdt0139_ecc TYPE TABLE OF zsdt0139.
DATA gt_zsdt0139_aux TYPE TABLE OF zsdt0139.

DATA gt_zsdt0140_s4 TYPE TABLE OF zsdt0140.
DATA gt_zsdt0140_ecc TYPE TABLE OF zsdt0140.
DATA gt_zsdt0140_aux TYPE TABLE OF zsdt0140.

DATA gt_zsdt0144_s4 TYPE TABLE OF zsdt0144.
DATA gt_zsdt0144_ecc TYPE TABLE OF zsdt0144.
DATA gt_zsdt0144_aux TYPE TABLE OF zsdt0144.

DATA gt_zsdt0163_s4 TYPE TABLE OF zsdt0163.
DATA gt_zsdt0163_ecc TYPE TABLE OF zsdt0163.
DATA gt_zsdt0163_aux TYPE TABLE OF zsdt0163.

DATA gt_zsdt0164_s4 TYPE TABLE OF zsdt0164.
DATA gt_zsdt0164_ecc TYPE TABLE OF zsdt0164.
DATA gt_zsdt0164_aux TYPE TABLE OF zsdt0164.

DATA gt_zsdt0218_s4 TYPE TABLE OF zsdt0218.
DATA gt_zsdt0218_ecc TYPE TABLE OF zsdt0218.
DATA gt_zsdt0218_aux TYPE TABLE OF zsdt0218.

DATA gt_zsdt0219_s4 TYPE TABLE OF zsdt0219.
DATA gt_zsdt0219_ecc TYPE TABLE OF zsdt0219.
DATA gt_zsdt0219_aux TYPE TABLE OF zsdt0219.

DATA gt_zsdt0302_s4 TYPE TABLE OF zsdt0302.
DATA gt_zsdt0302_ecc TYPE TABLE OF zsdt0302.
DATA gt_zsdt0302_aux TYPE TABLE OF zsdt0302.

DATA gt_zsdt0298_s4 TYPE TABLE OF zsdt0298.
DATA gt_zsdt0298_ecc TYPE TABLE OF zsdt0298.
DATA gt_zsdt0298_aux TYPE TABLE OF zsdt0298.

" 07 -
DATA gt_zppt0008_s4 TYPE TABLE OF zppt0008.
DATA gt_zppt0008_ecc TYPE TABLE OF zppt0008.
DATA gt_zppt0008_aux TYPE TABLE OF zppt0008.

DATA gt_zppt0009_s4 TYPE TABLE OF zppt0009.
DATA gt_zppt0009_ecc TYPE TABLE OF zppt0009.
DATA gt_zppt0009_aux TYPE TABLE OF zppt0009.

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS rb_01 RADIOBUTTON GROUP rb01 DEFAULT 'X' USER-COMMAND ucom.
  PARAMETERS rb_02 RADIOBUTTON GROUP rb01.
  PARAMETERS rb_03 RADIOBUTTON GROUP rb01.
  PARAMETERS rb_04 RADIOBUTTON GROUP rb01.
  PARAMETERS rb_05 RADIOBUTTON GROUP rb01.
  PARAMETERS rb_06 RADIOBUTTON GROUP rb01.
  PARAMETERS rb_07 RADIOBUTTON GROUP rb01.
SELECTION-SCREEN END OF BLOCK b1.

" DADOS SECUNDARIOS
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  " 01 - ZSDT0100
  SELECT-OPTIONS so_data FOR mara-laeda OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b2.

" TABELAS RELACIONADAS
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.

  PARAMETERS p_116_x1 AS CHECKBOX DEFAULT 'X'." 01
  PARAMETERS p_082_x3 AS CHECKBOX DEFAULT 'X'." 03

  PARAMETERS p_l82_x4 AS CHECKBOX DEFAULT 'X'." 04 0082 - PELA DT_LIBER
  PARAMETERS p_c82_x4 AS CHECKBOX DEFAULT 'X'." 04 0082 - PELA DT_CANC

  PARAMETERS p_150_x4 AS CHECKBOX DEFAULT 'X'." 04
  PARAMETERS p_82k_x4 AS CHECKBOX DEFAULT ''."'X'." 04

  PARAMETERS p_136_x5 AS CHECKBOX DEFAULT 'X'." 04
  PARAMETERS p_154_x5 AS CHECKBOX DEFAULT 'X'." 04

  PARAMETERS p_129_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_130_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_131_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_133_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_134_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_137_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_138_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_139_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_140_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_144_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_163_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_164_x6 AS CHECKBOX DEFAULT 'X'. " 07

  PARAMETERS p_218_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_219_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_302_x6 AS CHECKBOX DEFAULT 'X'. " 07
  PARAMETERS p_298_x6 AS CHECKBOX DEFAULT 'X'. " 07


  PARAMETERS p_008_x7 AS CHECKBOX DEFAULT 'X'. " 07
  "PARAMETERS p_009_x7 AS CHECKBOX DEFAULT 'X'. " 07

SELECTION-SCREEN END OF BLOCK b4.

" DADOS DO ALV
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS p_teste AS CHECKBOX DEFAULT 'X'.
  PARAMETERS p_sobre AS CHECKBOX.
  "PARAMETERS p_chave AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  "PERFORM f_refresh_screen.
  PERFORM f_preenche_data.
  "PERFORM f_control_screen.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_control_screen.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.
  PERFORM f_clear_all.
  PERFORM f_processa.

  IF p_teste = abap_true.
    PERFORM f_exibe_alv.
  ELSE.
    PERFORM f_inserir_direto USING space.
  ENDIF.

END-OF-SELECTION.
  PERFORM f_control_screen_reset.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_processa .

  DATA lv_json TYPE string.
  DATA lv_table TYPE tabname16.

  CASE 'X'.
    WHEN rb_01.

      IF p_116_x1 = abap_true.

        lv_table = 'ZSDT0116'.

        PERFORM f_get_ecc_table USING lv_table 'DT_APV' CHANGING lv_json.

        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0116_ecc ).

        PERFORM f_processa_alv_zsdt0100 USING lv_table.

      ENDIF.

*    WHEN rb_02.
    WHEN rb_03.

      IF p_082_x3 = abap_true.

        PERFORM f_get_ecc_table USING 'ZSDT0082' 'DT_SOL' CHANGING lv_json.
        REPLACE ALL OCCURRENCES OF '9999-12-31' IN lv_json WITH space.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0082_ecc ).

        "PERFORM f_get_0082_0082_key_ecc.

        PERFORM f_processa_alv_zsdt0079  USING 'ZSDT0082'.

      ENDIF.


    WHEN rb_04.

      CLEAR gt_dados_alv[].

      IF p_l82_x4 = abap_true.

        PERFORM f_get_ecc_table USING 'ZSDT0082' 'DT_LIBER' CHANGING lv_json.

        CLEAR gt_zsdt0082_aux[].
        REPLACE ALL OCCURRENCES OF '9999-12-31' IN lv_json WITH space.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0082_aux ).

        APPEND LINES OF gt_zsdt0082_aux TO gt_zsdt0082_ecc.

      ENDIF.

      IF p_c82_x4 = abap_true.

        " -----------------------------------------------------------------------------------------
        PERFORM f_get_ecc_table USING 'ZSDT0082' 'DT_CANC' CHANGING lv_json.

        CLEAR gt_zsdt0082_aux[].

        REPLACE ALL OCCURRENCES OF '9999-12-31' IN lv_json WITH space.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0082_aux ).

        APPEND LINES OF gt_zsdt0082_aux TO gt_zsdt0082_ecc.

      ENDIF.

      SORT gt_zsdt0082_ecc.

      DELETE ADJACENT DUPLICATES FROM gt_zsdt0082_ecc.

      IF p_82k_x4 = abap_false.
        PERFORM f_processa_alv_zsdt0079  USING 'ZSDT0082'.
      ENDIF.

      IF p_150_x4 = abap_true.

        " -----------------------------------------------------------------------------------------
        PERFORM f_get_ecc_table USING 'ZSDT0150' 'DT_REGISTRO' CHANGING lv_json.

        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0150_ecc ).

        PERFORM f_processa_alv_zsdt0081  USING 'ZSDT0150'.

      ENDIF.

      IF p_82k_x4 = abap_true.
        PERFORM f_get_0082_0150_key.
      ENDIF.

    WHEN rb_05.

      IF p_136_x5 = abap_true.

        PERFORM f_get_ecc_table USING 'ZSDT0136' '' CHANGING lv_json.

        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0136_ecc ).

        PERFORM f_processa_alv_zsdt0136.

      ENDIF.

      IF p_154_x5 = abap_true.

        PERFORM f_get_ecc_table USING 'ZSDT0154' '' CHANGING lv_json.

        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0154_ecc ).

        PERFORM f_processa_alv_zsdt0154.

      ENDIF.

    WHEN rb_06.

      " -----------------------------------------------------------------------------------------
      IF p_129_x6 = 'X'.

        CLEAR gt_zsdt0129_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0129' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0129_aux ).
        APPEND LINES OF gt_zsdt0129_aux TO gt_zsdt0129_ecc.

        CLEAR gt_zsdt0129_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0129' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0129_aux ).
        APPEND LINES OF gt_zsdt0129_aux TO gt_zsdt0129_ecc.

        PERFORM f_processa_alv_zsdt0129.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_130_x6 = 'X'.

        PERFORM f_get_ecc_table USING 'ZSDT0130' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0130_ecc ).

        PERFORM f_processa_alv_zsdt0130.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_131_x6 = 'X'.

        CLEAR gt_zsdt0131_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0131' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0131_aux ).
        APPEND LINES OF gt_zsdt0131_aux TO gt_zsdt0131_ecc.

        CLEAR gt_zsdt0131_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0131' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0131_aux ).
        APPEND LINES OF gt_zsdt0131_aux TO gt_zsdt0131_ecc.

        PERFORM f_processa_alv_zsdt0131.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_133_x6 = 'X'.

        CLEAR gt_zsdt0133_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0133' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0133_aux ).
        APPEND LINES OF gt_zsdt0133_aux TO gt_zsdt0133_ecc.

        CLEAR gt_zsdt0133_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0133' 'DATA_EDIT' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0133_aux ).
        APPEND LINES OF gt_zsdt0133_aux TO gt_zsdt0133_ecc.

        CLEAR gt_zsdt0133_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0133' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0133_aux ).
        APPEND LINES OF gt_zsdt0133_aux TO gt_zsdt0133_ecc.

        PERFORM f_processa_alv_zsdt0133.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_134_x6 = 'X'.

        CLEAR gt_zsdt0134_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0134' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0134_aux ).
        APPEND LINES OF gt_zsdt0134_aux TO gt_zsdt0134_ecc.

        CLEAR gt_zsdt0134_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0134' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0134_aux ).
        APPEND LINES OF gt_zsdt0134_aux TO gt_zsdt0134_ecc.

        PERFORM f_processa_alv_zsdt0134.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_137_x6 = 'X'.

        CLEAR gt_zsdt0137_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0137' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0137_aux ).
        APPEND LINES OF gt_zsdt0137_aux TO gt_zsdt0137_ecc.

        CLEAR gt_zsdt0137_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0137' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0137_aux ).
        APPEND LINES OF gt_zsdt0137_aux TO gt_zsdt0137_ecc.

        PERFORM f_processa_alv_zsdt0137.

      ENDIF.

      IF p_138_x6 = 'X'.

        CLEAR gt_zsdt0138_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0138' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0138_aux ).
        APPEND LINES OF gt_zsdt0138_aux TO gt_zsdt0138_ecc.

        CLEAR gt_zsdt0138_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0138' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0138_aux ).
        APPEND LINES OF gt_zsdt0138_aux TO gt_zsdt0138_ecc.

        PERFORM f_processa_alv_zsdt0138.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_139_x6 = 'X'.

        CLEAR gt_zsdt0139_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0139' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0139_aux ).
        APPEND LINES OF gt_zsdt0139_aux TO gt_zsdt0139_ecc.

        CLEAR gt_zsdt0139_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0139' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0139_aux ).
        APPEND LINES OF gt_zsdt0139_aux TO gt_zsdt0139_ecc.

        PERFORM f_processa_alv_zsdt0139.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_140_x6 = 'X'.

        CLEAR gt_zsdt0140_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0140' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0140_aux ).
        APPEND LINES OF gt_zsdt0140_aux TO gt_zsdt0140_ecc.

        CLEAR gt_zsdt0140_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0140' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0140_aux ).
        APPEND LINES OF gt_zsdt0140_aux TO gt_zsdt0140_ecc.

        PERFORM f_processa_alv_zsdt0140.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_144_x6 = 'X'.

        CLEAR gt_zsdt0144_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0144' '' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0144_aux ).
        APPEND LINES OF gt_zsdt0144_aux TO gt_zsdt0144_ecc.

        CLEAR gt_zsdt0144_aux.

        PERFORM f_processa_alv_zsdt0144.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_163_x6 = 'X'.

        CLEAR gt_zsdt0163_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0163' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0163_aux ).
        APPEND LINES OF gt_zsdt0163_aux TO gt_zsdt0163_ecc.

        CLEAR gt_zsdt0163_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0163' 'DATA_ATUAL_E' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0163_aux ).
        APPEND LINES OF gt_zsdt0163_aux TO gt_zsdt0163_ecc.

        PERFORM f_processa_alv_zsdt0163.

      ENDIF.

      " -----------------------------------------------------------------------------------------
      IF p_164_x6 = 'X'.

        CLEAR gt_zsdt0164_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0164' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0164_aux ).
        APPEND LINES OF gt_zsdt0164_aux TO gt_zsdt0164_ecc.

        CLEAR gt_zsdt0164_aux.

        PERFORM f_processa_alv_zsdt0164.

      ENDIF.

      "------------------------------------------------------------------------------------------
      IF p_218_x6 = 'X'.

        CLEAR gt_zsdt0218_ecc.

        PERFORM f_get_ecc_table USING 'ZSDT0218' 'DATA_ATUAL' CHANGING lv_json.

        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0218_aux ).

        APPEND LINES OF gt_zsdt0218_aux TO gt_zsdt0218_ecc.

*        CLEAR gt_zsdt0218_aux.
*        PERFORM f_get_ecc_table USING 'ZSDT0218' 'DATA_ATUAL_E' CHANGING lv_json.
*        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0218_aux ).
*        APPEND LINES OF gt_zsdt0218_aux TO gt_zsdt0218_ecc.

        PERFORM f_processa_alv_zsdt0218.

      ENDIF.

      "------------------------------------------------------------------------------------------
      IF p_219_x6 = 'X'.

        CLEAR gt_zsdt0219_ecc.

        PERFORM f_get_ecc_table USING 'ZSDT0219' 'DATA_ATUAL' CHANGING lv_json.

        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0219_aux ).

        APPEND LINES OF gt_zsdt0219_aux TO gt_zsdt0219_ecc.

        PERFORM f_processa_alv_zsdt0219.

      ENDIF.

      IF p_302_x6 = 'X'.

        CLEAR gt_zsdt0302_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0302' '' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0302_aux ).
        APPEND LINES OF gt_zsdt0302_aux TO gt_zsdt0302_ecc.

        PERFORM f_processa_alv_zsdt0302.

      ENDIF.

      IF p_298_x6 = 'X'.

        CLEAR gt_zsdt0298_ecc.
        PERFORM f_get_ecc_table USING 'ZSDT0298' 'DATA_ATUAL' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0298_aux ).
        APPEND LINES OF gt_zsdt0298_aux TO gt_zsdt0298_ecc.

        CLEAR gt_zsdt0298_aux.
        PERFORM f_get_ecc_table USING 'ZSDT0298' 'DT_CANC' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0298_aux ).
        APPEND LINES OF gt_zsdt0298_aux TO gt_zsdt0298_ecc.

        PERFORM f_processa_alv_zsdt0298.

      ENDIF.








    WHEN rb_07.

      IF p_008_x7 = 'X'.

        CLEAR gt_zppt0008_ecc[].

        PERFORM f_get_ecc_table USING 'ZPPT0008' 'CREATEDDT' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zppt0008_aux ).

        APPEND LINES OF gt_zppt0008_aux TO gt_zppt0008_ecc.

        CLEAR gt_zppt0008_aux[].

        PERFORM f_get_ecc_table USING 'ZPPT0008' 'CHANGEDDT' CHANGING lv_json.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zppt0008_aux ).

        APPEND LINES OF gt_zppt0008_aux TO gt_zppt0008_ecc.

        SORT gt_zppt0008_ecc.
        DELETE ADJACENT DUPLICATES FROM gt_zppt0008_ecc.

        " ESTAMOS RECONSTRUINDO A ZPPT0008, PQ TEM DATA NA CHAVE E VIMOS QUE O PROGRAMA NÃO MANTEM EXCLUIDAS
        " POR ISSO TEMOS QUE PEGAR O QUE FOI ACHADO DA ZPPT0008 E REFAZER A BUSCA COM A CHAVE PARCIAL VBELN E POSNR.
        PERFORM f_recontroi_zppt0008.

        PERFORM f_get_0009_0008_key.


      ENDIF.

*      IF p_009_x7 = 'X'.
*
*        CLEAR gt_zppt0009_ecc[].
*
*        PERFORM f_get_ecc_table USING 'ZPPT0009' 'DATA' CHANGING lv_json.
*        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zppt0009_aux ).
*
*        APPEND LINES OF gt_zppt0009_aux TO gt_zppt0009_ecc.
*
*        CLEAR gt_zppt0009_aux[].
*
*        PERFORM f_get_ecc_table USING 'ZPPT0009' 'DT_MODIFICACAO' CHANGING lv_json.
*        /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zppt0009_aux ).
*
*        APPEND LINES OF gt_zppt0009_aux TO gt_zppt0009_ecc.
*
*
*      ENDIF.

      PERFORM f_processa_alv_zpp0003 USING 'ZPPT0008' 'ZPPT0009'.

  ENDCASE.

ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
    WHEN 'F3'.
      SET SCREEN 0.
    WHEN '&IC1'.
      PERFORM f_hyperlink   USING rs_selfield.
    WHEN 'INSERIR'.
      PERFORM f_inserir_direto USING space.
    WHEN 'INSERIR_2'.
      PERFORM f_inserir_direto USING abap_true.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

  rs_selfield-refresh = 'X'.
  r_ucomm = '&REFRESH'.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_FUNCTION
*&---------------------------------------------------------------------*
FORM f_botao_function.

  sscrfields-functxt_01 = 'BOTAO 1'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_COMMAND
*&---------------------------------------------------------------------*
FORM f_botao_command.

  IF sy-ucomm = 'FC01'.
    "EXECUTA FUNÇÃO DO BOTAO 1
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  DATA lw_layout TYPE slis_layout_alv.
  DATA lw_variant TYPE disvariant.

  IF gt_dados_alv IS NOT INITIAL.

*    IF p_vari IS NOT INITIAL.
*      lw_variant-report = sy-repid.
*      lw_variant-variant = p_vari.
*    ENDIF.

    PERFORM f_monta_fieldcat.

    lw_layout-zebra             = abap_true.
    lw_layout-colwidth_optimize = abap_true.
    lw_layout-box_fieldname = gc_select_field.
    lw_layout-info_fieldname = 'COLOR'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'F_STATUS_SET'
        i_callback_user_command  = 'F_USER_COMMAND'
        is_layout                = lw_layout
        it_fieldcat              = gt_fieldcat
        i_save                   = 'A'
        is_variant               = lw_variant
      TABLES
        t_outtab                 = gt_dados_alv
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
      PERFORM f_mensagem_sistema.
    ENDIF.

  ELSE.
    MESSAGE s213(v4) DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
*&---------------------------------------------------------------------*
*&      Form  F_PFSTATUS
*&---------------------------------------------------------------------*
FORM f_status_set USING p_extab TYPE slis_t_extab.          "#EC CALLED

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "F_PFSTATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat.

  "LVC_FIELDCATALOG_MERGE
  SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_internal_tabname     = gc_internal_tab
      i_structure_name       = gc_struc_name
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = gc_icon_field.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext_ddic = 'Status'.
    <fs_fcat>-ddic_outputlen = 000010.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'REGISTRO'.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext_ddic = 'Registro ECC'.
    <fs_fcat>-ddic_outputlen = 000015.
    <fs_fcat>-hotspot = 'X'.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'QTDE_DIF'.

  IF sy-subrc EQ 0.
    <fs_fcat>-ddic_outputlen = 000010.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = gc_select_field.
  DELETE gt_fieldcat WHERE fieldname = 'COLOR'.
  DELETE gt_fieldcat WHERE fieldname = 'TABIX_ECC'.

  PERFORM f_coluna_edita USING 'MSGTX' 'Mensagem'.

  PERFORM f_coluna_edita USING 'COUNT_SAP' 'Notas SAP'.

  PERFORM f_coluna_edita USING 'TCODE' 'Transação'.
  PERFORM f_coluna_edita USING 'ZTABLE' 'Tabela'.

  PERFORM f_coluna_edita USING 'CHAVE_1' 'Key 1'.
  PERFORM f_coluna_edita USING 'CHAVE_2' 'Key 2'.
  PERFORM f_coluna_edita USING 'CHAVE_3' 'Key 3'.
  PERFORM f_coluna_edita USING 'CHAVE_4' 'Key 4'.
  PERFORM f_coluna_edita USING 'CHAVE_5' 'Key 5'.
  PERFORM f_coluna_edita USING 'CHAVE_6' 'Key 6'.
  PERFORM f_coluna_edita USING 'COLUNA_BASE' 'Filtro'.

  PERFORM f_coluna_edita USING 'EXISTE_ECC' 'ECC'.
  PERFORM f_coluna_edita USING 'EXISTE_S4' 'S4'.
  PERFORM f_coluna_edita USING 'TEM_DIF' 'Diferença'.
  PERFORM f_coluna_edita USING 'COLUNA_DIF' 'Coluna'.
  PERFORM f_coluna_edita USING 'VALOR_DIF' 'Vlr S4'.
  PERFORM f_coluna_edita USING 'QTDE_DIF' 'Qt.Dif'.
  PERFORM f_coluna_edita USING 'TTL_COL' 'Qt.Col'.
  PERFORM f_fieldcat_modi USING 'EXISTE_ECC' 'CHECKBOX' 'X' CHANGING gt_fieldcat.
  PERFORM f_fieldcat_modi USING 'EXISTE_S4' 'CHECKBOX' 'X' CHANGING gt_fieldcat.
  PERFORM f_fieldcat_modi USING 'TEM_DIF' 'CHECKBOX' 'X' CHANGING gt_fieldcat.

ENDFORM.                    " F_MONTA_FIELDCAT
"PERFORM f_fieldcat_modi USING 'PESO' 'EDIT' 'X' CHANGING lt_fieldcat.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
FORM f_fieldcat_modi USING p_fieldname TYPE slis_fieldname
                           p_column TYPE c
                           p_value TYPE any
                  CHANGING p_field_cat TYPE slis_t_fieldcat_alv.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  DATA(lv_name) = '<FS_FCAT>-' && p_column.

  ASSIGN (lv_name) TO FIELD-SYMBOL(<fs_colum>).

  CHECK sy-subrc EQ 0.

  <fs_colum> = p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema.

  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_s.

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_SISTEMA_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_insere.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING sy-msgty
           sy-msgid
           sy-msgno
           sy-msgv1
           sy-msgv2
           sy-msgv3
           sy-msgv4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE c
                           p_percent TYPE i.

  IF sy-batch = 'X'.

    IF strlen( p_text ) > 40.
      MESSAGE s000(d2) WITH p_text(40) p_text+40.
    ELSE.
      MESSAGE s000(d2) WITH p_text.
    ENDIF.

  ELSE.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = p_percent
        text       = p_text.

  ENDIF.

ENDFORM.
*& --------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
FORM f4_for_variant CHANGING f_vari TYPE slis_vari.

  DATA: lw_variant TYPE disvariant.

  lw_variant-variant = f_vari.
  lw_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = lw_variant
      i_save     = 'A'
    IMPORTING
      es_variant = lw_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    "f4_for_variant
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VARIANT
*&---------------------------------------------------------------------*
FORM default_variant CHANGING f_vari TYPE slis_vari.
  DATA: lw_variant TYPE disvariant.

  lw_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = lw_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc = 0.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    " DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DATA
*&---------------------------------------------------------------------*
FORM f_preenche_data .

  DATA(lv_ini) = '20220101'.
  DATA(lv_fim) = '20220501'.

  CHECK so_data[] IS INITIAL.

  "SUBTRACT 15 FROM lv_ini.
  "ADD 15 TO lv_fim.

  APPEND 'IBT' && lv_ini && lv_fim TO so_data.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
FORM f_hyperlink USING rs_selfield TYPE slis_selfield.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CHECK sy-subrc EQ 0.

  CASE rs_selfield-fieldname.
    WHEN 'REGISTRO'.
      PERFORM f_ver_regisro_din USING lw_saida_alv lw_saida_alv-tabix_ecc.
    WHEN 'CHAVE_1'.
      PERFORM f_abrir_se16 USING lw_saida_alv.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_HYPERLINK

"PERFORM f_mensagem_insere TABLES p_ret2
"USING 'E' 'ZMM' '000' 'SYSID' text-t02
"gw_034-logsys space space.

FORM f_mensagem_bapiret USING p_mess TYPE bapiret2.

  MESSAGE ID p_mess-id TYPE 'S' NUMBER p_mess-number
    WITH p_mess-message_v1 p_mess-message_v2
         p_mess-message_v3 p_mess-message_v4.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_EXIBE_POPUP
*&---------------------------------------------------------------------*
FORM f_mensagem_exibe_popup USING p_bapiret2_tab TYPE bapiret2_t.

  DATA: l_lines TYPE i.

  DESCRIBE TABLE p_bapiret2_tab LINES l_lines.

  IF l_lines <= 1 OR sy-batch = 'X'.

    LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

      MESSAGE ID <fs_ret2>-id
            TYPE 'S'
          NUMBER <fs_ret2>-number
            WITH <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4 DISPLAY LIKE <fs_ret2>-type.

    ENDLOOP.

  ELSE.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.

    LOOP AT p_bapiret2_tab ASSIGNING <fs_ret2>.

      IF <fs_ret2>-id IS INITIAL.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = <fs_ret2>-id
          "EXCEPTION_IF_NOT_ACTIVE  = 'X'
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
          "ZEILE                    = ' '
          "IMPORTING
          "ACT_SEVERITY             =
          "MAX_SEVERITY             =
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.     "#EC CI_SUBRC

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        OTHERS    = 5.     "#EC CI_SUBRC

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        "CORRECTIONS_OPTION          = ' '
        "CORRECTIONS_FUNC_TEXT       = ' '
        "LINE_FROM                   = ' '
        "LINE_TO                     = ' '
        "OBJECT                      = ' '
        "SEND_IF_ONE                 = ' '
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
        "MSG_SELECT_FUNC             = ' '
        "MSG_SELECT_FUNC_TEXT        = ' '
        "IMPORTING
        "CORRECTIONS_WANTED          =
        "E_EXIT_COMMAND              =
        "MSG_SELECTED                =
      EXCEPTIONS
        inconsistent_range  = 1
        no_messages         = 2
        OTHERS              = 3.     "#EC CI_SUBRC

  ENDIF.

ENDFORM.
* CONTROLE DE ATUALIZAÇÃO DE TELA DINAMICAMENTE

*    DATA lt_return TYPE TABLE OF ddshretval.
*    DATA lt_fields TYPE TABLE OF dynpread.
*
*    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*      EXPORTING
*        tabname           = 'ZTPP_009'
*        fieldname         = 'MATNR_DUMMY'
*        "searchhelp        = 'ZHPP_DUMMY'
*        "shlpparam         = 'MATNR_DUMMY'
*        "IMPORTING
*        "user_reset        =
*      TABLES
*        return_tab        = lt_return
*      EXCEPTIONS
*        field_not_found   = 1
*        no_help_for_field = 2
*        inconsistent_help = 3
*        no_values_found   = 4
*        OTHERS            = 5.
*
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>).
*
*      APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn>).
*
**      stepl
**
**      fieldinp
*
*      CASE <fs_ret>-fieldname.
*        WHEN 'WERKS'.
*          <fs_dyn>-fieldname = 'P_WERKS'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'ARBPL'.
*          <fs_dyn>-fieldname = 'P_ARBPL'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'VORNR'.
*
*          <fs_dyn>-fieldname = 'P_VORNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*        WHEN 'FLAG_DUMPS'.
*
*          APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn2>).
*
*          IF <fs_ret>-fieldval = 'X'.
*
*            <fs_dyn>-fieldname = 'P_DUM_S'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_N'.
*            <fs_dyn2>-fieldvalue = space.
*
*          ELSE.
*            <fs_dyn>-fieldname = 'P_DUM_N'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_S'.
*            <fs_dyn2>-fieldvalue = space.
*          ENDIF.
*
*        WHEN 'MATNR_DUMMY'.
*
*          <fs_dyn>-fieldname = 'P_MATNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*
*      ENDCASE.
*
*    ENDLOOP.
*
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*      EXPORTING
*        dyname               = '1000'
*        dynumb               = '1000'
*      TABLES
*        dynpfields           = lt_fields
*      EXCEPTIONS
*        invalid_abapworkarea = 1
*        invalid_dynprofield  = 2
*        invalid_dynproname   = 3
*        invalid_dynpronummer = 4
*        invalid_request      = 5
*        no_fielddescription  = 6
*        undefind_error       = 7
*        OTHERS               = 8.
*
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.


***AT SELECTION-SCREEN OUTPUT.
**
**    LOOP AT SCREEN.
**
**      IF screen-name CP '*P_WERKS*'
**        OR screen-name CP '*P_DUM_N*'
**        OR screen-name CP '*P_ARBPL*'
**        OR screen-name CP '*P_VORNR*'.
**
**        screen-input = 0.
**        MODIFY SCREEN.
**      ENDIF.
**
**    ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita  USING p_fieldname TYPE slis_fieldname
                           p_text TYPE scrtext_l.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-seltext_s = p_text.
  <fs_cat>-seltext_m = p_text.
  <fs_cat>-seltext_l = p_text.
  <fs_cat>-reptext_ddic = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.

  READ TABLE gt_dados_alv WITH KEY selec = 'X' TRANSPORTING NO FIELDS.

  IF sy-subrc NE 0.
    MESSAGE s851(v4) DISPLAY LIKE 'E'.
    p_error = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_INSERIR
*&---------------------------------------------------------------------*
FORM f_inserir.

  DATA lr_selec TYPE RANGE OF flag.

  DATA lv_ret.

  IF sy-batch IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING TEXT-t01 CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.

  ENDIF.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.





  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_mensagem_insere_txt USING i_type TYPE bapi_mtype
                                 p_string TYPE string.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 30
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        lv_msg1 = <fs_line>.
      WHEN 2.
        lv_msg2 = <fs_line>.
      WHEN 3.
        lv_msg3 = <fs_line>.
      WHEN 4.
        lv_msg4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING i_type
           'DS'
           '016'
           lv_msg1
           lv_msg2
           lv_msg3
           lv_msg4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_destination
*&---------------------------------------------------------------------*
FORM f_get_destination CHANGING cv_dest TYPE c.

  CASE sy-sysid.
    WHEN 'DEV'.
      cv_dest =  'DEV_ECC'.
    WHEN 'QAS'.
      cv_dest =  'QAS_ECC'.
    WHEN 'PRD'.
      cv_dest =  'PRD_ECC'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_zsdt0100
*&---------------------------------------------------------------------*
FORM f_busca_ecc_zsdt0100.

  DATA lv_dest TYPE char40.
  DATA lv_json TYPE string.
  DATA lv_table TYPE tabname16.
  DATA lv_date TYPE datum.

  DATA lt_param TYPE TABLE OF tvarvc.

  PERFORM f_get_destination CHANGING lv_dest.

  lv_table = 'ZSDT0116'.

  PERFORM f_so_date_to_param USING 'DT_APV' CHANGING lt_param.

  "lv_date = sy-datum - 360.

  "lt_param = VALUE #( ( name = 'DT_APV' sign = 'I' opti = 'BT' low = lv_date high = sy-datum ) ).

  CALL FUNCTION 'ZSD_INSUMOS_CONT_S4_SELECT' DESTINATION lv_dest
    EXPORTING
      iv_table        = lv_table
    IMPORTING
      ev_return       = lv_json
    TABLES
      it_select_param = lt_param.

  IF lv_json IS NOT INITIAL.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_json
*       jsonx            =
*       pretty_name      = PRETTY_MODE-NONE
*       assoc_arrays     = C_BOOL-FALSE
*       assoc_arrays_opt = C_BOOL-FALSE
*       name_mappings    =
*       conversion_exits = C_BOOL-FALSE
*       hex_as_base64    = C_BOOL-TRUE
      CHANGING
        data = gt_zsdt0116_ecc.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_busca_s4_zsdt0100
*&---------------------------------------------------------------------*
FORM f_busca_s4_zsdt0100 .

  CHECK gt_zsdt0116_ecc[] IS NOT INITIAL.

  SELECT * FROM zsdt0116
    INTO TABLE gt_zsdt0116_s4
      FOR ALL ENTRIES IN gt_zsdt0116_ecc
        WHERE seq = gt_zsdt0116_ecc-seq
          AND vbeln = gt_zsdt0116_ecc-vbeln
          AND posnr = gt_zsdt0116_ecc-posnr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_busca_s4_zsdt0100
*&---------------------------------------------------------------------*
FORM f_busca_s4_zsdt0079 .

  CHECK gt_zsdt0082_ecc[] IS NOT INITIAL.

  SELECT * FROM zsdt0082
    INTO TABLE gt_zsdt0082_s4
      FOR ALL ENTRIES IN gt_zsdt0082_ecc
        WHERE nro_sol = gt_zsdt0082_ecc-nro_sol
          AND seq     = gt_zsdt0082_ecc-seq
          AND vbeln   = gt_zsdt0082_ecc-vbeln
          AND posnr   = gt_zsdt0082_ecc-posnr.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0100
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0100 USING uv_table TYPE tabname16.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING uv_table.

  PERFORM f_get_table_info USING uv_table CHANGING lt_fields.

  LOOP AT gt_zsdt0116_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0100'.
    <fs_alv>-ztable = uv_table.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-seq.
    <fs_alv>-chave_2 = <fs_ecc>-vbeln.
    <fs_alv>-chave_3 = <fs_ecc>-posnr.

    READ TABLE gt_zsdt0116_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
      WITH KEY seq = <fs_ecc>-seq
      vbeln = <fs_ecc>-vbeln
      posnr = <fs_ecc>-posnr.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0100
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0079 USING uv_table TYPE tabname16.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING uv_table.

  PERFORM f_get_table_info USING uv_table CHANGING lt_fields.

  LOOP AT gt_zsdt0082_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0079'.
    <fs_alv>-ztable = 'ZSDT0082'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_sol.
    <fs_alv>-chave_2 = <fs_ecc>-seq.
    <fs_alv>-chave_3 = <fs_ecc>-vbeln.
    <fs_alv>-chave_4 = <fs_ecc>-posnr.

    READ TABLE gt_zsdt0082_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_sol = <fs_ecc>-nro_sol
                     seq = <fs_ecc>-seq
                   vbeln = <fs_ecc>-vbeln
                   posnr = <fs_ecc>-posnr.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    " se nao existir no S4, verifica se o nro_sol já está em uso
    IF <fs_alv>-existe_s4 = abap_false.

      IF rb_03 IS NOT INITIAL AND p_082_x3 IS NOT INITIAL.

        READ TABLE gt_zsdt0082_s4_check ASSIGNING FIELD-SYMBOL(<fs_key_0082>)
          WITH KEY nro_sol = <fs_ecc>-nro_sol.

      ELSEIF rb_04 IS NOT INITIAL AND p_l82_x4 IS NOT INITIAL.

        READ TABLE gt_zsdt0082_s4_check ASSIGNING <fs_key_0082>
          WITH KEY nro_sol = <fs_ecc>-nro_sol
                       seq = <fs_ecc>-seq.

      ELSEIF rb_04 IS NOT INITIAL.
        READ TABLE gt_zsdt0082_s4_check ASSIGNING <fs_key_0082>
          WITH KEY nro_sol = <fs_ecc>-nro_sol.
      ENDIF.

      IF sy-subrc EQ 0.
        <fs_alv>-icon = icon_red_light.
        <fs_alv>-msgtx = 'NRO_SOL já existe no ambiente'.
      ENDIF.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ver_registro
*&---------------------------------------------------------------------*
FORM f_ver_registro USING us_reg TYPE zsde_cont_insumos.

*  CHECK us_reg-chave_1 IS NOT INITIAL.
*
*  DATA(lv_table_name) = 'GT_' && us_reg-ztable &&'_ECC'
*


  IF us_reg-ztable = 'ZSDT0116'.

    READ TABLE gt_zsdt0116_ecc ASSIGNING FIELD-SYMBOL(<fs_0116_ecc>)
      WITH KEY seq = us_reg-chave_1
             vbeln = us_reg-chave_2
             posnr = us_reg-chave_3.

    IF sy-subrc EQ 0.
      cl_demo_output=>display( name = 'Registro ECC' data = <fs_0116_ecc> ).
    ENDIF.

  ELSEIF us_reg-ztable = 'ZSDT0082'.

    READ TABLE gt_zsdt0082_ecc ASSIGNING FIELD-SYMBOL(<fs_0082_ecc>)
      WITH KEY nro_sol = us_reg-chave_1
                  seq = us_reg-chave_2
                  vbeln = us_reg-chave_3
                  posnr = us_reg-chave_4.

    IF sy-subrc EQ 0.
      cl_demo_output=>display( name = 'Registro ECC' data = <fs_0082_ecc> ).
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_so_date_to_param
*&---------------------------------------------------------------------*
FORM f_so_date_to_param USING uv_param TYPE rvari_vnam
                     CHANGING ct_param TYPE tvarvc_t.

  LOOP AT so_data.

    ct_param = VALUE #( ( name = uv_param sign = so_data-sign opti = so_data-option low = so_data-low high = so_data-high ) ).

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_inserir_direto
*&---------------------------------------------------------------------*
FORM f_inserir_direto USING uv_indv TYPE c.

  CHECK gt_dados_alv IS NOT INITIAL.

  IF uv_indv = abap_true.

    IF NOT line_exists( gt_dados_alv[ selec = abap_true ] ).
      MESSAGE 'Selecionar ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.EXIT.
    ENDIF.

    DELETE gt_dados_alv WHERE selec = space.

  ENDIF.
  "  CLEAR gt_dados_alv[].

  CASE 'X'.
    WHEN rb_01.

      "PERFORM f_insere_s4_din USING 'ZSDT0116'.
      PERFORM f_insere_zsdt0116.
      " PERFORM f_processa_alv_zsdt0100 USING 'ZSDT0116'.

    WHEN rb_02.
    WHEN rb_03.
      PERFORM f_insere_zsdt0082.
      "PERFORM f_processa_alv_zsdt0079 USING 'ZSDT0082'.

    WHEN rb_04.
      PERFORM f_insere_zsdt0082.
      PERFORM f_insere_zsdt0150.
      "PERFORM f_processa_alv_zsdt0081 USING 'ZSDT0150'.
      "PERFORM f_processa_alv_zsdt0079 USING 'ZSDT0082'.

    WHEN rb_05.

      PERFORM f_insere_zsdt0136.
      PERFORM f_insere_zsdt0154.

    WHEN rb_06.

      PERFORM f_insere_zsdt0129.
      PERFORM f_insere_zsdt0130.
      PERFORM f_insere_zsdt0131.
      PERFORM f_insere_zsdt0133.
      PERFORM f_insere_zsdt0134.
      PERFORM f_insere_zsdt0137.
      PERFORM f_insere_zsdt0138.
      PERFORM f_insere_zsdt0139.
      PERFORM f_insere_zsdt0140.
      PERFORM f_insere_zsdt0144.
      PERFORM f_insere_zsdt0163.
      PERFORM f_insere_zsdt0164.

      PERFORM f_insere_zsdt0218.
      PERFORM f_insere_zsdt0219.
      PERFORM f_insere_zsdt0302.
      PERFORM f_insere_zsdt0298.



    WHEN rb_07.

      PERFORM f_insere_zppt0008.
      PERFORM f_insere_zppt0009.
      "PERFORM f_processa_alv_zpp0003 USING 'ZPPT0008' 'ZPPT0009'.

    WHEN OTHERS.
  ENDCASE.

  PERFORM f_clear_all.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0164
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0164 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0164 TYPE TABLE OF zsdt0164.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0164_ecc ASSIGNING FIELD-SYMBOL(<fs_0164>)
      WITH KEY seq = <fs_alv>-chave_1
              nro_cg = <fs_alv>-chave_2
              lifnr = <fs_alv>-chave_3.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0164> TO lt_zsdt0164.

  ENDLOOP.

  IF lt_zsdt0164[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0164' 50.

  MODIFY zsdt0164 FROM TABLE lt_zsdt0164.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0163
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0163 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0163 TYPE TABLE OF zsdt0163.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0163_ecc ASSIGNING FIELD-SYMBOL(<fs_0163>)
      WITH KEY bukrs = <fs_alv>-chave_1
               lifnr = <fs_alv>-chave_2.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0163> TO lt_zsdt0163.

  ENDLOOP.

  IF lt_zsdt0163[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0163' 50.

  MODIFY zsdt0163 FROM TABLE lt_zsdt0163.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0144
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0144 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0144 TYPE TABLE OF zsdt0144.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0144_ecc ASSIGNING FIELD-SYMBOL(<fs_0144>)
      WITH KEY nro_sol = <fs_alv>-chave_1
                seq = <fs_alv>-chave_2
                filial_resp = <fs_alv>-chave_3
                vbeln = <fs_alv>-chave_4.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0144> TO lt_zsdt0144.

  ENDLOOP.

  IF lt_zsdt0144[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0144' 50.

  MODIFY zsdt0144 FROM TABLE lt_zsdt0144.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0140
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0140 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0140 TYPE TABLE OF zsdt0140.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0140_ecc ASSIGNING FIELD-SYMBOL(<fs_0140>)
      WITH KEY nro_cgd = <fs_alv>-chave_1
                nro_sol = <fs_alv>-chave_2
                seq = <fs_alv>-chave_3.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0140> TO lt_zsdt0140.

  ENDLOOP.

  IF lt_zsdt0140[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0140' 50.

  MODIFY zsdt0140 FROM TABLE lt_zsdt0140.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0139
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0139 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0139 TYPE TABLE OF zsdt0139.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0139_ecc ASSIGNING FIELD-SYMBOL(<fs_0139>)
      WITH KEY nro_cgd = <fs_alv>-chave_1.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0139> TO lt_zsdt0139.

  ENDLOOP.

  IF lt_zsdt0139[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0139' 50.

  MODIFY zsdt0139 FROM TABLE lt_zsdt0139.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0138
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0138 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0138 TYPE TABLE OF zsdt0138.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0138_ecc ASSIGNING FIELD-SYMBOL(<fs_0138>)
      WITH KEY seq_cam = <fs_alv>-chave_1
              nro_sol = <fs_alv>-chave_2
              seq = <fs_alv>-chave_3
              filial_resp = <fs_alv>-chave_4.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0138> TO lt_zsdt0138.

  ENDLOOP.

  IF lt_zsdt0138[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0138' 50.

  MODIFY zsdt0138 FROM TABLE lt_zsdt0138.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0116
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0116 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0116 TYPE TABLE OF zsdt0116.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0116_ecc ASSIGNING FIELD-SYMBOL(<fs_0116>)
      WITH KEY seq = <fs_alv>-chave_1
             vbeln = <fs_alv>-chave_2
             posnr = <fs_alv>-chave_3.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0116> TO lt_zsdt0116.

  ENDLOOP.

  IF lt_zsdt0116[] IS INITIAL.
    "MESSAGE s016(ds) WITH 'Os registros em vermelhos foram' 'excluidos da exibição' DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0116' 50.

  MODIFY zsdt0116 FROM TABLE lt_zsdt0116.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0116
*&---------------------------------------------------------------------*
FORM f_insere_s4_din USING uv_table TYPE tabname16.

  DATA lv_apaga TYPE c.
  DATA lv_tab_name TYPE string.

  FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.

  lv_tab_name = 'GT_' && uv_table && '_ECC[]'.

  ASSIGN (lv_tab_name) TO <fs_table>.

  CHECK sy-subrc EQ 0.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    CLEAR lv_apaga.

    IF <fs_alv>-coluna_dif IS NOT INITIAL.

      IF p_sobre IS INITIAL.
        lv_apaga = abap_true.
      ENDIF.

    ELSE.

      IF <fs_alv>-existe_s4 IS NOT INITIAL.
        lv_apaga = abap_true.
      ENDIF.

    ENDIF.

    IF lv_apaga = abap_true.
      DELETE <fs_table> INDEX <fs_alv>-tabix_ecc.
    ENDIF.

  ENDLOOP.

  IF <fs_table>[] IS INITIAL.
    "MESSAGE s016(ds) WITH 'Os registros já existem no' 'S4 completos' DISPLAY LIKE 'W'.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros...' 50.

  MODIFY (uv_table) FROM TABLE <fs_table>.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0082
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0082 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0082 TYPE TABLE OF zsdt0082.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
    WHERE ztable = 'ZSDT0082' AND icon = icon_yellow_light.

    READ TABLE gt_zsdt0082_ecc ASSIGNING FIELD-SYMBOL(<fs_0082>)
      WITH KEY nro_sol = <fs_alv>-chave_1
               seq = <fs_alv>-chave_2
               vbeln = <fs_alv>-chave_3
               posnr = <fs_alv>-chave_4.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0082> TO lt_zsdt0082.

  ENDLOOP.

  IF lt_zsdt0082[] IS INITIAL.
    "MESSAGE s016(ds) WITH 'Os registros em vermelhos foram' 'excluidos da exibição' DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0082' 50.

  MODIFY zsdt0082 FROM TABLE lt_zsdt0082.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_zsdt0100
*&---------------------------------------------------------------------*
FORM f_busca_ecc_zsdt0079.

  DATA lv_dest TYPE char40.
  DATA lv_json TYPE string.
  DATA lv_table TYPE tabname16.
  DATA lv_date TYPE datum.

  DATA lt_param TYPE TABLE OF tvarvc.

  PERFORM f_get_destination CHANGING lv_dest.

  lv_table = 'ZSDT0082'.

  PERFORM f_so_date_to_param USING 'DT_SOL' CHANGING lt_param.

  CALL FUNCTION 'ZSD_INSUMOS_CONT_S4_SELECT' DESTINATION lv_dest
    EXPORTING
      iv_table        = lv_table
    IMPORTING
      ev_return       = lv_json
    TABLES
      it_select_param = lt_param.

  IF lv_json IS NOT INITIAL.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_json
*       jsonx            =
*       pretty_name      = PRETTY_MODE-NONE
*       assoc_arrays     = C_BOOL-FALSE
*       assoc_arrays_opt = C_BOOL-FALSE
*       name_mappings    =
*       conversion_exits = C_BOOL-FALSE
*       hex_as_base64    = C_BOOL-TRUE
      CHANGING
        data = gt_zsdt0082_ecc.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_zsdt0100
*&---------------------------------------------------------------------*
FORM f_busca_ecc_zsdt0081.

  DATA lv_dest TYPE char40.
  DATA lv_json TYPE string.
  DATA lv_table TYPE tabname16.
  DATA lv_date TYPE datum.

  DATA lt_param TYPE TABLE OF tvarvc.

  PERFORM f_get_destination CHANGING lv_dest.

  lv_table = 'ZSDT0150'.

  PERFORM f_so_date_to_param USING 'DT_REGISTRO' CHANGING lt_param.

  CALL FUNCTION 'ZSD_INSUMOS_CONT_S4_SELECT' DESTINATION lv_dest
    EXPORTING
      iv_table        = lv_table
    IMPORTING
      ev_return       = lv_json
    TABLES
      it_select_param = lt_param.

  IF lv_json IS NOT INITIAL.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_json
*       jsonx            =
*       pretty_name      = PRETTY_MODE-NONE
*       assoc_arrays     = C_BOOL-FALSE
*       assoc_arrays_opt = C_BOOL-FALSE
*       name_mappings    =
*       conversion_exits = C_BOOL-FALSE
*       hex_as_base64    = C_BOOL-TRUE
      CHANGING
        data = gt_zsdt0150_ecc.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_ecc_table
*&---------------------------------------------------------------------*
FORM f_get_ecc_table USING uv_table TYPE tabname16
                           uv_data_param TYPE rvari_vnam
                  CHANGING cv_json TYPE string.

  DATA lv_dest TYPE char40.
  DATA lv_json TYPE string.
  DATA lv_date TYPE datum.
  DATA lv_flag.
  DATA lt_param TYPE TABLE OF tvarvc.

  "PERFORM f_get_flag_tab USING uv_table CHANGING lv_flag.

  "CHECK lv_flag IS NOT INITIAL.

  CLEAR cv_json.

  PERFORM f_get_destination CHANGING lv_dest.

  IF uv_data_param IS NOT INITIAL.
    PERFORM f_so_date_to_param USING uv_data_param CHANGING lt_param.
  ENDIF.

  CALL FUNCTION 'ZSD_INSUMOS_CONT_S4_SELECT' DESTINATION lv_dest
    EXPORTING
      iv_table        = uv_table
    IMPORTING
      ev_return       = cv_json
    TABLES
      it_select_param = lt_param.

  REPLACE ALL OCCURRENCES OF '--' IN cv_json WITH space.
  REPLACE ALL OCCURRENCES OF '9999-12-31' IN lv_json WITH space.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_busca_s4_zsdt0100
*&---------------------------------------------------------------------*
FORM f_busca_s4_zsdt0081 .

  CHECK gt_zsdt0150_ecc[] IS NOT INITIAL.

  SELECT * FROM zsdt0150
    INTO TABLE gt_zsdt0150_s4
      FOR ALL ENTRIES IN gt_zsdt0150_ecc
        WHERE nro_sol = gt_zsdt0150_ecc-nro_sol
          AND seq     = gt_zsdt0150_ecc-seq
          AND vbeln   = gt_zsdt0150_ecc-vbeln
          AND posnr   = gt_zsdt0150_ecc-posnr
          AND dt_registro = gt_zsdt0150_ecc-dt_registro
          AND hr_registro = gt_zsdt0150_ecc-hr_registro.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0100
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0081 USING uv_table TYPE tabname16.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING uv_table.

  PERFORM f_get_table_info USING uv_table CHANGING lt_fields.

  LOOP AT gt_zsdt0150_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0081'.
    <fs_alv>-ztable = 'ZSDT0150'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_sol.
    <fs_alv>-chave_2 = <fs_ecc>-seq.
    <fs_alv>-chave_3 = <fs_ecc>-vbeln.
    <fs_alv>-chave_4 = <fs_ecc>-posnr.
    <fs_alv>-chave_5 = <fs_ecc>-dt_registro.
    <fs_alv>-chave_6 = <fs_ecc>-hr_registro.

    READ TABLE gt_zsdt0150_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_sol = <fs_ecc>-nro_sol
                     seq = <fs_ecc>-seq
                   vbeln = <fs_ecc>-vbeln
                   posnr = <fs_ecc>-posnr
             dt_registro = <fs_ecc>-dt_registro " 15.10.2023 - RAMON
             hr_registro = <fs_ecc>-hr_registro. " 15.10.2023 - RAMON

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_desativa_bloco
*&---------------------------------------------------------------------*
FORM f_desativa_bloco USING uv_bloco TYPE i.

  DATA lv_name TYPE c LENGTH 4 VALUE '*_X'.

  lv_name = lv_name && uv_bloco.

  LOOP AT SCREEN.

    "screen-input = 1.

    IF screen-name CP lv_name.

      screen-input = 0.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_control_screen
*&---------------------------------------------------------------------*
FORM f_control_screen.

  CASE 'X'.
    WHEN rb_01.
      PERFORM f_desativa_bloco USING: 2,3,4,5,6,7.
    WHEN rb_02.
      PERFORM f_desativa_bloco USING: 1,3,4,5,6,7.
    WHEN rb_03.
      PERFORM f_desativa_bloco USING: 1,2,4,5,6,7.
    WHEN rb_04.
      PERFORM f_desativa_bloco USING: 1,2,3,5,6,7.
    WHEN rb_05.
      PERFORM f_desativa_bloco USING: 1,2,3,4,6,7.
    WHEN rb_06.
      PERFORM f_desativa_bloco USING: 1,2,3,4,5,7.
    WHEN rb_07.
      PERFORM f_desativa_bloco USING: 1,2,3,4,5,6.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_select_data_s4
*&---------------------------------------------------------------------*
FORM f_select_data_s4 USING uv_table TYPE tabname16.

  CASE uv_table.
    WHEN 'ZSDT0116'.

      CHECK gt_zsdt0116_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0116
        INTO TABLE gt_zsdt0116_s4
          FOR ALL ENTRIES IN gt_zsdt0116_ecc
            WHERE seq = gt_zsdt0116_ecc-seq
              AND vbeln = gt_zsdt0116_ecc-vbeln
              AND posnr = gt_zsdt0116_ecc-posnr.


    WHEN 'ZSDT0082'.

      CHECK gt_zsdt0082_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0082
        INTO TABLE gt_zsdt0082_s4
          FOR ALL ENTRIES IN gt_zsdt0082_ecc
            WHERE nro_sol = gt_zsdt0082_ecc-nro_sol
              AND seq     = gt_zsdt0082_ecc-seq
              AND vbeln   = gt_zsdt0082_ecc-vbeln
              AND posnr   = gt_zsdt0082_ecc-posnr.

      "IF p_chave IS NOT INITIAL.

      SELECT * FROM zsdt0082
        INTO TABLE gt_zsdt0082_s4_check
          FOR ALL ENTRIES IN gt_zsdt0082_ecc
            WHERE nro_sol = gt_zsdt0082_ecc-nro_sol.

      IF gt_zsdt0082_s4_check IS NOT INITIAL.

        " exclui os registro que são iguais as que estao inserindo,
        " o filtro campo a campo pega a diferença se houver
        LOOP AT gt_zsdt0082_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>).

          DELETE gt_zsdt0082_s4_check
            WHERE nro_sol = <fs_s4>-nro_sol
              AND seq     = <fs_s4>-seq
              AND vbeln   = <fs_s4>-vbeln
              AND posnr   = <fs_s4>-posnr.

        ENDLOOP.

      ENDIF.

      "ENDIF.

    WHEN 'ZSDT0150'.

      CHECK gt_zsdt0150_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0150
        INTO TABLE gt_zsdt0150_s4
          FOR ALL ENTRIES IN gt_zsdt0150_ecc
            WHERE nro_sol = gt_zsdt0150_ecc-nro_sol
              AND seq     = gt_zsdt0150_ecc-seq
              AND vbeln   = gt_zsdt0150_ecc-vbeln
              AND posnr   = gt_zsdt0150_ecc-posnr
              AND dt_registro = gt_zsdt0150_ecc-dt_registro
              AND hr_registro = gt_zsdt0150_ecc-hr_registro.

    WHEN 'ZSDT0136'.

      SELECT * FROM zsdt0136
      INTO TABLE gt_zsdt0136_s4
        FOR ALL ENTRIES IN gt_zsdt0136_ecc
          WHERE werks = gt_zsdt0136_ecc-werks
            AND safra = gt_zsdt0136_ecc-safra.

    WHEN 'ZSDT0154'.

      SELECT * FROM zsdt0154
      INTO TABLE gt_zsdt0154_s4
        FOR ALL ENTRIES IN gt_zsdt0154_ecc
          WHERE id_branch = gt_zsdt0154_ecc-id_branch
            AND nr_safra = gt_zsdt0154_ecc-nr_safra.

    WHEN 'ZSDT0129'.

      CHECK gt_zsdt0129_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0129
        INTO TABLE gt_zsdt0129_s4
          FOR ALL ENTRIES IN gt_zsdt0129_ecc
            WHERE nro_lote = gt_zsdt0129_ecc-nro_lote.

    WHEN 'ZSDT0130'.

      CHECK gt_zsdt0130_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0130
        INTO TABLE gt_zsdt0130_s4
          FOR ALL ENTRIES IN gt_zsdt0130_ecc
            WHERE nro_lote = gt_zsdt0130_ecc-nro_lote
              AND nro_sol  = gt_zsdt0130_ecc-nro_sol
              AND seq      = gt_zsdt0130_ecc-seq
              AND kunnr    = gt_zsdt0130_ecc-kunnr.

    WHEN 'ZSDT0131'.

      CHECK gt_zsdt0131_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0131
        INTO TABLE gt_zsdt0131_s4
          FOR ALL ENTRIES IN gt_zsdt0131_ecc
            WHERE nro_lote = gt_zsdt0131_ecc-nro_lote
              AND nro_sol  = gt_zsdt0131_ecc-nro_sol
              AND seq      = gt_zsdt0131_ecc-seq
              AND kunnr    = gt_zsdt0131_ecc-kunnr
              AND vbeln    = gt_zsdt0131_ecc-vbeln
              AND posnr    = gt_zsdt0131_ecc-posnr.

    WHEN 'ZSDT0133'.

      CHECK gt_zsdt0133_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0133
        INTO TABLE gt_zsdt0133_s4
          FOR ALL ENTRIES IN gt_zsdt0133_ecc
            WHERE nro_cg = gt_zsdt0133_ecc-nro_cg.

    WHEN 'ZSDT0134'.

      CHECK gt_zsdt0134_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0134
        INTO TABLE gt_zsdt0134_s4
          FOR ALL ENTRIES IN gt_zsdt0134_ecc
            WHERE vbeln = gt_zsdt0134_ecc-vbeln
              AND posnr = gt_zsdt0134_ecc-posnr
              AND charg = gt_zsdt0134_ecc-charg
              AND nro_cg = gt_zsdt0134_ecc-nro_cg
              AND nr_fase = gt_zsdt0134_ecc-nr_fase
              AND nr_rot = gt_zsdt0134_ecc-nr_rot.

    WHEN 'ZSDT0137'.

      CHECK gt_zsdt0137_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0137
        INTO TABLE gt_zsdt0137_s4
          FOR ALL ENTRIES IN gt_zsdt0137_ecc
            WHERE nro_sol = gt_zsdt0137_ecc-nro_sol
              AND seq = gt_zsdt0137_ecc-seq
              AND filial_resp = gt_zsdt0137_ecc-filial_resp.

    WHEN 'ZSDT0138'.

      CHECK gt_zsdt0138_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0138
        INTO TABLE gt_zsdt0138_s4
          FOR ALL ENTRIES IN gt_zsdt0138_ecc
            WHERE seq_cam = gt_zsdt0138_ecc-seq_cam
              AND nro_sol = gt_zsdt0138_ecc-nro_sol
              AND seq = gt_zsdt0138_ecc-seq
              AND filial_resp = gt_zsdt0138_ecc-filial_resp.

    WHEN 'ZSDT0139'.

      CHECK gt_zsdt0139_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0139
        INTO TABLE gt_zsdt0139_s4
          FOR ALL ENTRIES IN gt_zsdt0139_ecc
            WHERE nro_cgd = gt_zsdt0139_ecc-nro_cgd.

    WHEN 'ZSDT0140'.

      CHECK gt_zsdt0140_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0140
        INTO TABLE gt_zsdt0140_s4
          FOR ALL ENTRIES IN gt_zsdt0140_ecc
            WHERE nro_cgd = gt_zsdt0140_ecc-nro_cgd
              AND nro_sol = gt_zsdt0140_ecc-nro_sol
              AND seq = gt_zsdt0140_ecc-seq.

    WHEN 'ZSDT0144'.

      CHECK gt_zsdt0144_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0144
        INTO TABLE gt_zsdt0144_s4
          FOR ALL ENTRIES IN gt_zsdt0144_ecc
            WHERE nro_sol = gt_zsdt0144_ecc-nro_sol
              AND seq = gt_zsdt0144_ecc-seq
              AND filial_resp = gt_zsdt0144_ecc-filial_resp
              AND vbeln = gt_zsdt0144_ecc-vbeln.

    WHEN 'ZPPT0008'.

      CHECK gt_zppt0008_ecc[] IS NOT INITIAL.

      SELECT * FROM zppt0008
        INTO TABLE gt_zppt0008_s4
          FOR ALL ENTRIES IN gt_zppt0008_ecc
            WHERE vbeln = gt_zppt0008_ecc-vbeln
              AND posnr = gt_zppt0008_ecc-posnr
              AND edatu = gt_zppt0008_ecc-edatu.

    WHEN 'ZSDT0163'.

      CHECK gt_zsdt0163_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0163
        INTO TABLE gt_zsdt0163_s4
          FOR ALL ENTRIES IN gt_zsdt0163_ecc
            WHERE bukrs = gt_zsdt0163_ecc-bukrs
              AND lifnr = gt_zsdt0163_ecc-lifnr.

    WHEN 'ZSDT0164'.

      CHECK gt_zsdt0164_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0164
        INTO TABLE gt_zsdt0164_s4
          FOR ALL ENTRIES IN gt_zsdt0164_ecc
            WHERE seq = gt_zsdt0164_ecc-seq
              AND nro_cg = gt_zsdt0164_ecc-nro_cg
              AND lifnr = gt_zsdt0164_ecc-lifnr.

    WHEN 'ZSDT0218'.

      CHECK gt_zsdt0218_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0218
        INTO TABLE gt_zsdt0218_s4
          FOR ALL ENTRIES IN gt_zsdt0218_ecc
            WHERE numeroreceita = gt_zsdt0218_ecc-numeroreceita
              AND numeropedido = gt_zsdt0218_ecc-numeropedido
              AND cpfrt = gt_zsdt0218_ecc-cpfrt
              AND receitakey = gt_zsdt0218_ecc-receitakey.

    WHEN 'ZSDT0219'.

      CHECK gt_zsdt0219_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0219
        INTO TABLE gt_zsdt0219_s4
          FOR ALL ENTRIES IN gt_zsdt0219_ecc
            WHERE numeroreceita = gt_zsdt0219_ecc-numeroreceita
              AND numeropedido = gt_zsdt0219_ecc-numeropedido
              AND cpfrt = gt_zsdt0219_ecc-cpfrt
              AND codigomapa = gt_zsdt0219_ecc-codigomapa
              AND receitakey = gt_zsdt0219_ecc-receitakey
              AND produto_id_agriq = gt_zsdt0219_ecc-produto_id_agriq.

    WHEN 'ZSDT0302'.

      CHECK gt_zsdt0302_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0302
        INTO TABLE gt_zsdt0302_s4
          FOR ALL ENTRIES IN gt_zsdt0302_ecc
            WHERE nro_cgd = gt_zsdt0302_ecc-nro_cgd
              AND ch_referencia = gt_zsdt0302_ecc-ch_referencia.

    WHEN 'ZSDT0298'.

      CHECK gt_zsdt0298_ecc[] IS NOT INITIAL.

      SELECT * FROM zsdt0298
        INTO TABLE gt_zsdt0298_s4
          FOR ALL ENTRIES IN gt_zsdt0298_ecc
            WHERE nro_cgd = gt_zsdt0298_ecc-nro_cgd
              AND ch_referencia = gt_zsdt0298_ecc-ch_referencia.

    WHEN 'ZPPT0009'.

      CHECK gt_zppt0009_ecc[] IS NOT INITIAL.

      SELECT * FROM zppt0009
        INTO TABLE gt_zppt0009_s4
          FOR ALL ENTRIES IN gt_zppt0009_ecc
            WHERE vbeln = gt_zppt0009_ecc-vbeln
              AND posnr = gt_zppt0009_ecc-posnr
              AND ordem_carreg = gt_zppt0009_ecc-ordem_carreg.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0100
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0112 USING uv_table1 TYPE tabname16
                                   uv_table2 TYPE tabname16
                                   uv_table3 TYPE tabname16
                                   uv_table4 TYPE tabname16
                                   uv_table5 TYPE tabname16
                                   uv_table6 TYPE tabname16.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING uv_table1.

  PERFORM f_get_table_info USING uv_table1 CHANGING lt_fields.

  CLEAR gt_dados_alv[].

  LOOP AT gt_zsdt0129_ecc ASSIGNING FIELD-SYMBOL(<fs_0129_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = uv_table1.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_0129_ecc>-nro_lote.

    READ TABLE gt_zsdt0129_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_lote = <fs_0129_ecc>-nro_lote.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_0129_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_0129_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

  PERFORM f_select_data_s4 USING uv_table2.
  PERFORM f_get_table_info USING uv_table2 CHANGING lt_fields.

  " -------------------------------------------------------------------------- ZSDT0130
  LOOP AT gt_zsdt0130_ecc ASSIGNING FIELD-SYMBOL(<fs_0130_ecc>).

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING <fs_alv>.

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = uv_table2.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_0130_ecc>-nro_lote.

    READ TABLE gt_zsdt0130_s4 ASSIGNING FIELD-SYMBOL(<fs_0130_s4>)
        WITH KEY nro_lote = <fs_0130_ecc>-nro_lote
                 nro_sol  = <fs_0130_ecc>-nro_sol
                 seq      = <fs_0130_ecc>-seq
                 kunnr    = <fs_0130_ecc>-kunnr.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_0130_ecc> NE <fs_0130_s4>.

        LOOP AT lt_fields ASSIGNING <fs_fields>.

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_0130_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<fs_0130_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO <fs_col_ecc>.
          ASSIGN (lv_field_s4) TO <fs_col_s4>.

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

  PERFORM f_select_data_s4 USING uv_table3.
  PERFORM f_get_table_info USING uv_table3 CHANGING lt_fields.

  " -------------------------------------------------------------------------- ZSDT0131
  LOOP AT gt_zsdt0131_ecc ASSIGNING FIELD-SYMBOL(<fs_0131_ecc>).

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING <fs_alv>.

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = uv_table3.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_0131_ecc>-nro_lote.

    READ TABLE gt_zsdt0131_s4 ASSIGNING FIELD-SYMBOL(<fs_0131_s4>)
        WITH KEY nro_lote = <fs_0131_ecc>-nro_lote
                 nro_sol  = <fs_0131_ecc>-nro_sol
                 seq      = <fs_0131_ecc>-seq
                 kunnr    = <fs_0131_ecc>-kunnr
                 vbeln    = <fs_0131_ecc>-vbeln
                 posnr    = <fs_0131_ecc>-posnr.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_0131_ecc> NE <fs_0131_s4>.

        LOOP AT lt_fields ASSIGNING <fs_fields>.

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_0131_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<fs_0131_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO <fs_col_ecc>.
          ASSIGN (lv_field_s4) TO <fs_col_s4>.

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

  PERFORM f_select_data_s4 USING uv_table4.
  PERFORM f_get_table_info USING uv_table4 CHANGING lt_fields.

  " -------------------------------------------------------------------------- ZSDT0133
  LOOP AT gt_zsdt0133_ecc ASSIGNING FIELD-SYMBOL(<fs_0133_ecc>).

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING <fs_alv>.

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = uv_table4.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_0133_ecc>-nro_cg.

    READ TABLE gt_zsdt0133_s4 ASSIGNING FIELD-SYMBOL(<fs_0133_s4>)
        WITH KEY nro_cg = <fs_0133_ecc>-nro_cg.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_0133_ecc> NE <fs_0133_s4>.

        LOOP AT lt_fields ASSIGNING <fs_fields>.

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_0133_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<fs_0133_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO <fs_col_ecc>.
          ASSIGN (lv_field_s4) TO <fs_col_s4>.

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

  PERFORM f_select_data_s4 USING uv_table5.
  PERFORM f_get_table_info USING uv_table5 CHANGING lt_fields.

  " -------------------------------------------------------------------------- ZSDT0134
  LOOP AT gt_zsdt0134_ecc ASSIGNING FIELD-SYMBOL(<fs_0134_ecc>).

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING <fs_alv>.

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = uv_table5.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_0134_ecc>-vbeln.
    <fs_alv>-chave_2 = <fs_0134_ecc>-posnr.
    <fs_alv>-chave_3 = <fs_0134_ecc>-charg.
    <fs_alv>-chave_4 = <fs_0134_ecc>-nro_cg.
    <fs_alv>-chave_5 = <fs_0134_ecc>-nr_fase.
    <fs_alv>-chave_6 = <fs_0134_ecc>-nr_rot.

    READ TABLE gt_zsdt0134_s4 ASSIGNING FIELD-SYMBOL(<fs_0134_s4>)
        WITH KEY vbeln   = <fs_0134_ecc>-vbeln
                 posnr   = <fs_0134_ecc>-posnr
                 charg   = <fs_0134_ecc>-charg
                 nro_cg  = <fs_0134_ecc>-nro_cg
                 nr_fase = <fs_0134_ecc>-nr_fase
                 nr_rot  = <fs_0134_ecc>-nr_rot.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_0134_ecc> NE <fs_0134_s4>.

        LOOP AT lt_fields ASSIGNING <fs_fields>.

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_0134_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<fs_0134_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO <fs_col_ecc>.
          ASSIGN (lv_field_s4) TO <fs_col_s4>.

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

  PERFORM f_select_data_s4 USING uv_table6.
  PERFORM f_get_table_info USING uv_table6 CHANGING lt_fields.

  " -------------------------------------------------------------------------- ZSDT0134
  LOOP AT gt_zsdt0139_ecc ASSIGNING FIELD-SYMBOL(<fs_0139_ecc>).

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING <fs_alv>.

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = uv_table6.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_0139_ecc>-nro_cgd.

    READ TABLE gt_zsdt0139_s4 ASSIGNING FIELD-SYMBOL(<fs_0139_s4>)
        WITH KEY nro_cgd   = <fs_0139_ecc>-nro_cgd.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_0139_ecc> NE <fs_0139_s4>.

        LOOP AT lt_fields ASSIGNING <fs_fields>.

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_0139_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_0139_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO <fs_col_ecc>.
          ASSIGN (lv_field_s4) TO <fs_col_s4>.

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_TABLE_INFO
*&---------------------------------------------------------------------*
FORM f_get_table_info USING uv_table TYPE tabname16
                   CHANGING ct_fields TYPE dbfieldtab.

  DATA lv_table TYPE tabname.

  CLEAR ct_fields.

  lv_table = uv_table.

  CALL FUNCTION 'DB_GET_TABLE_FIELDS'
    EXPORTING
      tabname  = lv_table
    TABLES
      dbfields = ct_fields.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zpp0003
*&---------------------------------------------------------------------*
FORM f_processa_alv_zpp0003 USING uv_table1 TYPE tabname16
                                  uv_table2 TYPE tabname16.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING uv_table1.

  PERFORM f_get_table_info USING uv_table1 CHANGING lt_fields.

  CLEAR gt_dados_alv[].

  SORT gt_zppt0008_ecc.

  DELETE ADJACENT DUPLICATES FROM gt_zppt0008_ecc.

  LOOP AT gt_zppt0008_ecc ASSIGNING FIELD-SYMBOL(<fs_0008_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.
    <fs_alv>-tcode = 'ZPP0003'.
    <fs_alv>-ztable = uv_table1.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_0008_ecc>-vbeln.
    <fs_alv>-chave_2 = <fs_0008_ecc>-posnr.
    <fs_alv>-chave_3 = <fs_0008_ecc>-edatu.

    READ TABLE gt_zppt0008_s4 ASSIGNING FIELD-SYMBOL(<fs_0008_s4>)
        WITH KEY vbeln = <fs_0008_ecc>-vbeln
                 posnr = <fs_0008_ecc>-posnr
                 edatu = <fs_0008_ecc>-edatu.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_0008_ecc> NE <fs_0008_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_0008_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_0008_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

  " -------------------------------------------------------------------------- ZPPT0009

  PERFORM f_select_data_s4 USING uv_table2.

  PERFORM f_get_table_info USING uv_table2 CHANGING lt_fields.

  SORT gt_zppt0009_ecc.

  DELETE ADJACENT DUPLICATES FROM gt_zppt0009_ecc.

  LOOP AT gt_zppt0009_ecc ASSIGNING FIELD-SYMBOL(<fs_0009_ecc>).

    lv_tabix = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING <fs_alv>.

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.
    <fs_alv>-tcode = 'ZPP0003'.
    <fs_alv>-ztable = uv_table2.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_0009_ecc>-vbeln.
    <fs_alv>-chave_2 = <fs_0009_ecc>-posnr.
    <fs_alv>-chave_3 = <fs_0009_ecc>-ordem_carreg.

    READ TABLE gt_zppt0009_s4 ASSIGNING FIELD-SYMBOL(<fs_0009_s4>)
        WITH KEY vbeln = <fs_0009_ecc>-vbeln
                 posnr = <fs_0009_ecc>-posnr
                 ordem_carreg = <fs_0009_ecc>-ordem_carreg.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_0009_ecc> NE <fs_0009_s4>.

        LOOP AT lt_fields ASSIGNING <fs_fields>.

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_0009_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_0009_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO <fs_col_ecc>.
          ASSIGN (lv_field_s4) TO <fs_col_s4>.

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0150
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0150 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0150 TYPE TABLE OF zsdt0150.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)
      WHERE ztable = 'ZSDT0150' AND icon = icon_yellow_light.

    READ TABLE gt_zsdt0150_ecc ASSIGNING FIELD-SYMBOL(<fs_0150>)
      WITH KEY nro_sol = <fs_alv>-chave_1
               seq     = <fs_alv>-chave_2
               vbeln   = <fs_alv>-chave_3
               posnr   = <fs_alv>-chave_4
               dt_registro = <fs_alv>-chave_5
               hr_registro = <fs_alv>-chave_6.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0150> TO lt_zsdt0150.

  ENDLOOP.

  IF lt_zsdt0150[] IS INITIAL.
    "MESSAGE s016(ds) WITH 'Os registros em vermelhos foram' 'excluidos da exibição' DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0150' 50.

  MODIFY zsdt0150 FROM TABLE lt_zsdt0150.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ver_regisro_din
*&---------------------------------------------------------------------*
FORM f_ver_regisro_din USING us_reg TYPE zsde_cont_insumos
                             uv_index TYPE syindex.

  FIELD-SYMBOLS <fs_tab> TYPE STANDARD TABLE.

  CHECK us_reg-chave_1 IS NOT INITIAL.

  DATA(lv_table_name) = 'GT_' && us_reg-ztable &&'_ECC'.

  ASSIGN (lv_table_name) TO <fs_tab>.

  CHECK <fs_tab> IS ASSIGNED.

  READ TABLE <fs_tab> ASSIGNING FIELD-SYMBOL(<fs_field>) INDEX uv_index.

  CHECK sy-subrc EQ 0.

  cl_demo_output=>display( name = 'Registro ECC' data = <fs_field> ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_ZPPT0008
*&---------------------------------------------------------------------*
FORM f_insere_zppt0008 .

  DATA lv_apaga TYPE c.

  DATA lt_zppt0008 TYPE TABLE OF zppt0008.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)." WHERE icon <> icon_red_light.

    READ TABLE gt_zppt0008_ecc ASSIGNING FIELD-SYMBOL(<fs_0008>)
      WITH KEY vbeln = <fs_alv>-chave_1
               posnr = <fs_alv>-chave_2
               edatu = <fs_alv>-chave_3.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0008> TO lt_zppt0008.

  ENDLOOP.

  IF lt_zppt0008[] IS INITIAL.
    "MESSAGE s016(ds) WITH 'Os registros em vermelhos foram' 'excluidos da exibição' DISPLAY LIKE 'E'. EXIT.
    EXIT.
  ENDIF.

  LOOP AT lt_zppt0008 ASSIGNING <fs_0008>.

    PERFORM f_sap_indicator USING 'Excluindo registros ZPPT0008' 50.

    DELETE FROM zppt0008 WHERE vbeln = <fs_0008>-vbeln.

  ENDLOOP.

  COMMIT WORK AND WAIT.

  PERFORM f_sap_indicator USING 'Inserindo registros ZPPT0008' 50.
  MODIFY zppt0008 FROM TABLE lt_zppt0008.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_ZPPT0009
*&---------------------------------------------------------------------*
FORM f_insere_zppt0009 .

  DATA lv_apaga TYPE c.

  DATA lt_zppt0009 TYPE TABLE OF zppt0009.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>)." WHERE icon <> icon_red_light.

    READ TABLE gt_zppt0009_ecc ASSIGNING FIELD-SYMBOL(<fs_0009>)
      WITH KEY vbeln = <fs_alv>-chave_1
               posnr = <fs_alv>-chave_2
               ordem_carreg = <fs_alv>-chave_3.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0009> TO lt_zppt0009.

  ENDLOOP.

  IF lt_zppt0009[] IS INITIAL.
    "MESSAGE s016(ds) WITH 'Os registros em vermelhos foram' 'excluidos da exibição' DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  LOOP AT lt_zppt0009 ASSIGNING <fs_0009>.

    PERFORM f_sap_indicator USING 'Excluindo registros ZPPT0009' 50.

    DELETE FROM zppt0009 WHERE vbeln = <fs_0009>-vbeln.

  ENDLOOP.

  COMMIT WORK AND WAIT.

  MODIFY zppt0009 FROM TABLE lt_zppt0009.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0129
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0129 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0129 TYPE TABLE OF zsdt0129.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0129_ecc ASSIGNING FIELD-SYMBOL(<fs_0129>)
      WITH KEY nro_lote = <fs_alv>-chave_1.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0129> TO lt_zsdt0129.

  ENDLOOP.

  IF lt_zsdt0129[] IS INITIAL.
    EXIT.
  ENDIF.

  CHECK lt_zsdt0129 IS NOT INITIAL.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0129' 50.

  MODIFY zsdt0129 FROM TABLE lt_zsdt0129.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0130
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0130 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0130 TYPE TABLE OF zsdt0130.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0130_ecc ASSIGNING FIELD-SYMBOL(<fs_0130>)
      WITH KEY nro_lote = <fs_alv>-chave_1
               nro_sol  = <fs_alv>-chave_2
               seq      = <fs_alv>-chave_3
               kunnr    = <fs_alv>-chave_4.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0130> TO lt_zsdt0130.

  ENDLOOP.

  IF lt_zsdt0130[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0130' 50.

  MODIFY zsdt0130 FROM TABLE lt_zsdt0130.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0131
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0131 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0131 TYPE TABLE OF zsdt0131.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0131_ecc ASSIGNING FIELD-SYMBOL(<fs_0131>)
      WITH KEY nro_lote = <fs_alv>-chave_1
               nro_sol  = <fs_alv>-chave_2
               seq      = <fs_alv>-chave_3
               kunnr    = <fs_alv>-chave_4
               vbeln    = <fs_alv>-chave_5
               posnr    = <fs_alv>-chave_6.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0131> TO lt_zsdt0131.

  ENDLOOP.

  IF lt_zsdt0131[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0131' 50.

  MODIFY zsdt0131 FROM TABLE lt_zsdt0131.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0133
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0133 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0133 TYPE TABLE OF zsdt0133.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0133_ecc ASSIGNING FIELD-SYMBOL(<fs_0133>)
      WITH KEY nro_cg = <fs_alv>-chave_1.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0133> TO lt_zsdt0133.

  ENDLOOP.

  IF lt_zsdt0133[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0133' 50.

  MODIFY zsdt0133 FROM TABLE lt_zsdt0133.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0134
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0134 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0134 TYPE TABLE OF zsdt0134.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0134_ecc ASSIGNING FIELD-SYMBOL(<fs_0134>)
      WITH KEY vbeln = <fs_alv>-chave_1
                posnr = <fs_alv>-chave_2
                charg = <fs_alv>-chave_3
                nro_cg = <fs_alv>-chave_4
                nr_fase = <fs_alv>-chave_5
                nr_rot = <fs_alv>-chave_6.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0134> TO lt_zsdt0134.

  ENDLOOP.

  IF lt_zsdt0134[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0134' 50.

  MODIFY zsdt0134 FROM TABLE lt_zsdt0134.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_control_screen_reset
*&---------------------------------------------------------------------*
FORM f_control_screen_reset .

  LOOP AT SCREEN.

    screen-input = 1.

    MODIFY SCREEN.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_clear_all
*&---------------------------------------------------------------------*
FORM f_clear_all .

  CLEAR: gt_dados_alv, gt_zsdt0116_ecc,
         gt_zsdt0116_s4, gt_zsdt0082_s4,
         gt_zsdt0082_ecc, gt_zsdt0150_s4,
         gt_zsdt0150_ecc, gt_zsdt0129_s4,
         gt_zsdt0129_ecc, gt_zsdt0130_s4,
         gt_zsdt0130_ecc, gt_zsdt0131_s4,
         gt_zsdt0131_ecc, gt_zsdt0133_s4,
         gt_zsdt0133_ecc, gt_zsdt0134_s4,
         gt_zsdt0134_ecc, gt_zsdt0139_s4,
         gt_zsdt0139_ecc, gt_zppt0008_s4,
         gt_zppt0008_ecc, gt_zppt0009_s4,
         gt_zppt0009_ecc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0137
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0137 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0137 TYPE TABLE OF zsdt0137.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0137_ecc ASSIGNING FIELD-SYMBOL(<fs_0137>)
      WITH KEY nro_sol = <fs_alv>-chave_1
                   seq = <fs_alv>-chave_2
           filial_resp = <fs_alv>-chave_3.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0137> TO lt_zsdt0137.

  ENDLOOP.

  IF lt_zsdt0137[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0137' 50.

  MODIFY zsdt0137 FROM TABLE lt_zsdt0137.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0129
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0129 .

  CHECK p_129_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0129'.

  PERFORM f_get_table_info USING 'ZSDT0129' CHANGING lt_fields.

  SORT gt_zsdt0129_ecc.

  DELETE ADJACENT DUPLICATES FROM gt_zsdt0129_ecc.

  LOOP AT gt_zsdt0129_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0129'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_lote.

    READ TABLE gt_zsdt0129_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_lote = <fs_ecc>-nro_lote.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0130
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0130 .

  CHECK p_130_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0130'.

  PERFORM f_get_table_info USING 'ZSDT0130' CHANGING lt_fields.

  SORT gt_zsdt0130_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0130_ecc.

  LOOP AT gt_zsdt0130_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0130'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_lote.
    <fs_alv>-chave_2 = <fs_ecc>-nro_sol.
    <fs_alv>-chave_3 = <fs_ecc>-seq.
    <fs_alv>-chave_4 = <fs_ecc>-kunnr.

    READ TABLE gt_zsdt0130_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_lote = <fs_ecc>-nro_lote
                 nro_sol  = <fs_ecc>-nro_sol
                 seq      = <fs_ecc>-seq
                 kunnr    = <fs_ecc>-kunnr.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0131
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0131 .

  CHECK p_131_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0131'.

  PERFORM f_get_table_info USING 'ZSDT0131' CHANGING lt_fields.

  SORT gt_zsdt0131_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0131_ecc.

  LOOP AT gt_zsdt0131_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0131'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_lote.
    <fs_alv>-chave_2 = <fs_ecc>-nro_sol.
    <fs_alv>-chave_3 = <fs_ecc>-seq.
    <fs_alv>-chave_4 = <fs_ecc>-kunnr.
    <fs_alv>-chave_5 = <fs_ecc>-vbeln.
    <fs_alv>-chave_6 = <fs_ecc>-posnr.

    READ TABLE gt_zsdt0131_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_lote = <fs_ecc>-nro_lote
                 nro_sol  = <fs_ecc>-nro_sol
                 seq      = <fs_ecc>-seq
                 kunnr    = <fs_ecc>-kunnr
                 vbeln    = <fs_ecc>-vbeln
                 posnr    = <fs_ecc>-posnr.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0133
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0133 .

  CHECK p_133_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0133'.

  PERFORM f_get_table_info USING 'ZSDT0133' CHANGING lt_fields.

  SORT gt_zsdt0133_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0133_ecc.

  LOOP AT gt_zsdt0133_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0133'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_cg.

    READ TABLE gt_zsdt0133_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_cg = <fs_ecc>-nro_cg.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0134
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0134 .

  CHECK p_134_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0134'.

  PERFORM f_get_table_info USING 'ZSDT0134' CHANGING lt_fields.

  SORT gt_zsdt0134_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0134_ecc.

  LOOP AT gt_zsdt0134_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0134'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-vbeln.
    <fs_alv>-chave_2 = <fs_ecc>-posnr.
    <fs_alv>-chave_3 = <fs_ecc>-charg.
    <fs_alv>-chave_4 = <fs_ecc>-nro_cg.
    <fs_alv>-chave_5 = <fs_ecc>-nr_fase.
    <fs_alv>-chave_6 = <fs_ecc>-nr_rot.

    READ TABLE gt_zsdt0134_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY vbeln   = <fs_ecc>-vbeln
                 posnr   = <fs_ecc>-posnr
                 charg   = <fs_ecc>-charg
                 nro_cg  = <fs_ecc>-nro_cg
                 nr_fase = <fs_ecc>-nr_fase
                 nr_rot  = <fs_ecc>-nr_rot.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0139
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0139 .

  CHECK p_139_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0139'.

  PERFORM f_get_table_info USING 'ZSDT0139' CHANGING lt_fields.

  SORT gt_zsdt0139_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0139_ecc.

  LOOP AT gt_zsdt0139_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0139'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_cgd.

    READ TABLE gt_zsdt0139_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_cgd = <fs_ecc>-nro_cgd.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0137
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0137 .

  CHECK p_137_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0137'.

  PERFORM f_get_table_info USING 'ZSDT0137' CHANGING lt_fields.

  SORT gt_zsdt0137_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0137_ecc.

  LOOP AT gt_zsdt0137_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0137'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_sol.
    <fs_alv>-chave_2 = <fs_ecc>-seq.
    <fs_alv>-chave_3 = <fs_ecc>-filial_resp.

    READ TABLE gt_zsdt0137_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_sol = <fs_ecc>-nro_sol
                 seq = <fs_ecc>-seq
                 filial_resp = <fs_ecc>-filial_resp.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0138
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0138 .

  CHECK p_138_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0138'.

  PERFORM f_get_table_info USING 'ZSDT0138' CHANGING lt_fields.

  SORT gt_zsdt0138_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0138_ecc.

  LOOP AT gt_zsdt0138_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0138'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-seq_cam.
    <fs_alv>-chave_2 = <fs_ecc>-nro_sol.
    <fs_alv>-chave_3 = <fs_ecc>-seq.
    <fs_alv>-chave_4 = <fs_ecc>-filial_resp.

    READ TABLE gt_zsdt0138_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY seq_cam = <fs_ecc>-seq_cam
                 nro_sol = <fs_ecc>-nro_sol
                 seq = <fs_ecc>-seq
                 filial_resp = <fs_ecc>-filial_resp.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0140
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0140 .

  CHECK p_140_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0140'.

  PERFORM f_get_table_info USING 'ZSDT0140' CHANGING lt_fields.

  SORT gt_zsdt0140_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0140_ecc.

  LOOP AT gt_zsdt0140_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0140'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_cgd.
    <fs_alv>-chave_2 = <fs_ecc>-nro_sol.
    <fs_alv>-chave_3 = <fs_ecc>-seq.

    READ TABLE gt_zsdt0140_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_cgd = <fs_ecc>-nro_cgd
                 nro_sol = <fs_ecc>-nro_sol
                 seq = <fs_ecc>-seq.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0144
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0144 .

  CHECK p_144_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0144'.

  PERFORM f_get_table_info USING 'ZSDT0144' CHANGING lt_fields.

  SORT gt_zsdt0144_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0144_ecc.

  LOOP AT gt_zsdt0144_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0144'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_sol.
    <fs_alv>-chave_2 = <fs_ecc>-seq.
    <fs_alv>-chave_3 = <fs_ecc>-filial_resp.
    <fs_alv>-chave_4 = <fs_ecc>-vbeln.

    READ TABLE gt_zsdt0144_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_sol = <fs_ecc>-nro_sol
                 seq = <fs_ecc>-seq
                 filial_resp = <fs_ecc>-filial_resp
                 vbeln = <fs_ecc>-vbeln.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0164
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0164 .

  CHECK p_164_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0164'.

  PERFORM f_get_table_info USING 'ZSDT0164' CHANGING lt_fields.

  SORT gt_zsdt0164_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0164_ecc.

  LOOP AT gt_zsdt0164_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0164'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-seq.
    <fs_alv>-chave_2 = <fs_ecc>-nro_cg.
    <fs_alv>-chave_3 = <fs_ecc>-lifnr.

    READ TABLE gt_zsdt0164_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY seq = <fs_ecc>-seq
                 nro_cg = <fs_ecc>-nro_cg
                 lifnr = <fs_ecc>-lifnr.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0136
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0136 .

  CHECK p_136_x5 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0136'.

  PERFORM f_get_table_info USING 'ZSDT0136' CHANGING lt_fields.

  "CLEAR gt_dados_alv[].

  LOOP AT gt_zsdt0136_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0116'.
    <fs_alv>-ztable = 'ZSDT0136'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-werks.
    <fs_alv>-chave_2 = <fs_ecc>-safra.

    READ TABLE gt_zsdt0136_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY werks = <fs_ecc>-werks
                 safra = <fs_ecc>-safra.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    IF <fs_alv>-existe_s4 = abap_true.

      IF <fs_s4>-seq_atual > <fs_ecc>-seq_atual.

        <fs_alv>-icon = icon_red_light.
        <fs_alv>-msgtx = 'Sequencia S4 é maior que do ECC'.

      ENDIF.

    ENDIF.


    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0154
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0154 .

  CHECK p_154_x5 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0154'.

  PERFORM f_get_table_info USING 'ZSDT0154' CHANGING lt_fields.

  "CLEAR gt_dados_alv[].

  LOOP AT gt_zsdt0154_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0116'.
    <fs_alv>-ztable = 'ZSDT0154'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-id_branch.
    <fs_alv>-chave_2 = <fs_ecc>-nr_safra.

    READ TABLE gt_zsdt0154_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY id_branch = <fs_ecc>-id_branch
                 nr_safra = <fs_ecc>-nr_safra.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    IF <fs_alv>-existe_s4 = abap_true.

      IF <fs_s4>-seq_atual > <fs_ecc>-seq_atual.

        <fs_alv>-icon = icon_red_light.
        <fs_alv>-msgtx = 'Sequencia S4 é maior que do ECC'.

      ENDIF.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_flag_tab
*&---------------------------------------------------------------------*
FORM f_get_flag_tab USING iv_tabname TYPE tabname16
                 CHANGING cv_tab_field TYPE c.

  DATA lv_flag.
  DATA lv_field TYPE c LENGTH 8.
  DATA lv_tabnumber TYPE c LENGTH 10.

  CHECK iv_tabname IS NOT INITIAL.

  CASE abap_true.
    WHEN rb_01.
      lv_flag = '1'.
    WHEN rb_02.
      lv_flag = '2'.
    WHEN rb_03.
      lv_flag = '3'.
    WHEN rb_04.
      lv_flag = '4'.
    WHEN rb_05.
      lv_flag = '5'.
    WHEN rb_06.
      lv_flag = '6'.
    WHEN rb_07.
      lv_flag = '7'.
  ENDCASE.

  lv_tabnumber = iv_tabname.

  lv_tabnumber = iv_tabname+5.

  lv_field = 'p_' && lv_tabnumber && '_x' && lv_flag.

  ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_check>).

  CHECK sy-subrc EQ 0.

  cv_tab_field = <fs_check>.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_0082_key
*&---------------------------------------------------------------------*
FORM f_get_0082_0150_key .

  DATA lv_json TYPE string.
  DATA lv_dest TYPE char40.
  DATA lt_param TYPE tvarvc_t.

  CLEAR gt_zsdt0082_aux.

  LOOP AT gt_zsdt0150_ecc ASSIGNING FIELD-SYMBOL(<fs_0150>).

    APPEND INITIAL LINE TO lt_param ASSIGNING FIELD-SYMBOL(<fs_param>).

    <fs_param>-name = 'NRO_SOL'.
    <fs_param>-sign = 'I'.
    <fs_param>-opti = 'EQ'.
    <fs_param>-low = <fs_0150>-nro_sol.
    <fs_param>-type = 'N'. "< ----LISTA

  ENDLOOP.

  SORT lt_param BY low ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_param.

  CHECK lt_param IS NOT INITIAL.

  PERFORM f_get_destination CHANGING lv_dest.

  CALL FUNCTION 'ZSD_INSUMOS_CONT_S4_SELECT' DESTINATION lv_dest
    EXPORTING
      iv_table        = 'ZSDT0082'
    IMPORTING
      ev_return       = lv_json
    TABLES
      it_select_param = lt_param.

  REPLACE ALL OCCURRENCES OF '--' IN lv_json WITH space.

  REPLACE ALL OCCURRENCES OF '9999-12-31' IN lv_json WITH space.
  CHECK lv_json IS NOT INITIAL.

  "CLEAR gt_zsdt0082_ecc.

  /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zsdt0082_aux ).

  LOOP AT gt_zsdt0150_ecc ASSIGNING <fs_0150>.

    READ TABLE gt_zsdt0082_aux ASSIGNING FIELD-SYMBOL(<fs_0082_aux>)
      WITH KEY nro_sol = <fs_0150>-nro_sol
                   seq = <fs_0150>-seq
                 vbeln = <fs_0150>-vbeln
                 posnr = <fs_0150>-posnr.

    " só fica com a chave que encontrar na 0082
    CHECK sy-subrc EQ 0.

    APPEND <fs_0082_aux> TO gt_zsdt0082_ecc.

  ENDLOOP.

  "APPEND LINES OF gt_zsdt0082_aux TO gt_zsdt0082_ecc.

  SORT gt_zsdt0082_ecc.

  DELETE ADJACENT DUPLICATES FROM gt_zsdt0082_ecc.

  PERFORM f_processa_alv_zsdt0079  USING 'ZSDT0082'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_0082_key
*&---------------------------------------------------------------------*
FORM f_get_0009_0008_key .

  DATA lv_json TYPE string.
  DATA lv_dest TYPE char40.
  DATA lt_param TYPE tvarvc_t.

  CLEAR gt_zppt0009_aux.

  LOOP AT gt_zppt0008_ecc ASSIGNING FIELD-SYMBOL(<fs_0008>).

    APPEND INITIAL LINE TO lt_param ASSIGNING FIELD-SYMBOL(<fs_param>).

    <fs_param>-name = 'VBELN'.
    <fs_param>-sign = 'I'.
    <fs_param>-opti = 'EQ'.
    <fs_param>-low = <fs_0008>-vbeln.
    <fs_param>-type = 'N'. "< ----LISTA

  ENDLOOP.

  SORT lt_param BY low ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_param.

  CHECK lt_param IS NOT INITIAL.

  PERFORM f_get_destination CHANGING lv_dest.

  CALL FUNCTION 'ZSD_INSUMOS_CONT_S4_SELECT' DESTINATION lv_dest
    EXPORTING
      iv_table        = 'ZPPT0009'
    IMPORTING
      ev_return       = lv_json
    TABLES
      it_select_param = lt_param.

  REPLACE ALL OCCURRENCES OF '--' IN lv_json WITH space.

  REPLACE ALL OCCURRENCES OF '9999-12-31' IN lv_json WITH space.
  CHECK lv_json IS NOT INITIAL.

  "CLEAR gt_ZPPT0009_ecc.

  /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zppt0009_aux ).

  LOOP AT gt_zppt0008_ecc ASSIGNING <fs_0008>.

    LOOP AT gt_zppt0009_aux ASSIGNING FIELD-SYMBOL(<fs_0009_aux>)
      WHERE vbeln = <fs_0008>-vbeln
        AND posnr = <fs_0008>-posnr.

      APPEND <fs_0009_aux> TO gt_zppt0009_ecc.

    ENDLOOP.

  ENDLOOP.

  SORT gt_zppt0009_ecc.

  DELETE ADJACENT DUPLICATES FROM gt_zppt0009_ecc.

  "PERFORM f_processa_alv_zsdt0079  USING 'ZPPT0009'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0136
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0136 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0136 TYPE TABLE OF zsdt0136.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0136_ecc ASSIGNING FIELD-SYMBOL(<fs_0136>)
      WITH KEY werks = <fs_alv>-chave_1
               safra = <fs_alv>-chave_2.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0136> TO lt_zsdt0136.

  ENDLOOP.

  IF lt_zsdt0136[] IS INITIAL.
    "MESSAGE s016(ds) WITH 'Os registros em vermelhos foram' 'excluidos da exibição' DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros zsdt0136' 50.

  MODIFY zsdt0136 FROM TABLE lt_zsdt0136.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0154
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0154 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0154 TYPE TABLE OF zsdt0154.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0154_ecc ASSIGNING FIELD-SYMBOL(<fs_0154>)
      WITH KEY id_branch = <fs_alv>-chave_1
               nr_safra = <fs_alv>-chave_2.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0154> TO lt_zsdt0154.

  ENDLOOP.

  IF lt_zsdt0154[] IS INITIAL.
    "MESSAGE s016(ds) WITH 'Os registros em vermelhos foram' 'excluidos da exibição' DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros zsdt0154' 50.

  MODIFY zsdt0154 FROM TABLE lt_zsdt0154.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PROCESSA_ALV_ZSDT0163
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0163.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 30.
  DATA lv_field_s4 TYPE c LENGTH 30.

  PERFORM f_select_data_s4 USING 'ZSDT0163'.

  PERFORM f_get_table_info USING 'ZSDT0163' CHANGING lt_fields.

  SORT gt_zsdt0163_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0163_ecc.

  LOOP AT gt_zsdt0163_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0163'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-bukrs.
    <fs_alv>-chave_2 = <fs_ecc>-lifnr.

    READ TABLE gt_zsdt0163_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY bukrs = <fs_ecc>-bukrs
                 lifnr = <fs_ecc>-lifnr.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ABRIR_SE16
*&---------------------------------------------------------------------*
FORM f_abrir_se16 USING cs_alv TYPE zsde_cont_insumos.

  DATA lt_fields TYPE dbfieldtab.
  DATA lv_num TYPE c.
  DATA lv_colum TYPE c LENGTH 30.

  DATA lr_selfields TYPE TABLE OF se16n_seltab.
  DATA lv_tab TYPE se16n_tab.
  DATA ls_field LIKE LINE OF lr_selfields.

  PERFORM f_get_table_info
    USING cs_alv-ztable
  CHANGING lt_fields.

  lv_tab = cs_alv-ztable.

  DELETE lt_fields WHERE name = 'MANDT'.
  DELETE lt_fields WHERE keyflag <> 'X'.

  CHECK sy-subrc EQ 0.

  DO 6 TIMES.

    lv_num = sy-index.

    lv_colum = 'CS_ALV-CHAVE_' && lv_num.

    ASSIGN (lv_colum) TO FIELD-SYMBOL(<fs_colum>).

    CHECK sy-subrc EQ 0.

    READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<fs_field>)
    INDEX lv_num.

    CHECK sy-subrc EQ 0.

    CLEAR ls_field.
    ls_field-sign = 'I'.
    ls_field-option = 'EQ'.
    ls_field-field = <fs_field>-name.
    ls_field-low = <fs_colum>.
    APPEND ls_field TO lr_selfields.

  ENDDO.

  CALL FUNCTION 'SE16N_INTERFACE'
    EXPORTING
      i_tab        = lv_tab
      i_edit       = abap_false
      i_sapedit    = abap_false
      i_tech_names = 'X'
    TABLES
      it_selfields = lr_selfields
      "it_output_fields = lt_output_fields
    EXCEPTIONS
      no_values    = 1
      OTHERS       = 2.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_recontroi_zppt0008
*&---------------------------------------------------------------------*
FORM f_recontroi_zppt0008 .

  DATA lv_json TYPE string.
  DATA lv_dest TYPE char40.
  DATA lt_param TYPE tvarvc_t.

  " 1° - BACKUP DO QUE JA ENCONTROU
  gt_zppt0008_aux =  gt_zppt0008_ecc.

  " 2° - CLEAR NA GERAL
  CLEAR gt_zppt0008_ecc.

  " 3° - GERA UMA LISTA DE TUDO QUE ENCONTROU
  LOOP AT gt_zppt0008_aux ASSIGNING FIELD-SYMBOL(<fs_0008>).

    APPEND INITIAL LINE TO lt_param ASSIGNING FIELD-SYMBOL(<fs_param>).

    <fs_param>-name = 'VBELN'.
    <fs_param>-sign = 'I'.
    <fs_param>-opti = 'EQ'.
    <fs_param>-low = <fs_0008>-vbeln.
    <fs_param>-type = 'N'. "< ----LISTA

  ENDLOOP.

  SORT lt_param BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_param.

  CHECK lt_param IS NOT INITIAL.

  PERFORM f_get_destination CHANGING lv_dest.

  CALL FUNCTION 'ZSD_INSUMOS_CONT_S4_SELECT' DESTINATION lv_dest
    EXPORTING
      iv_table        = 'ZPPT0008'
    IMPORTING
      ev_return       = lv_json
    TABLES
      it_select_param = lt_param.

  REPLACE ALL OCCURRENCES OF '--' IN lv_json WITH space.

  REPLACE ALL OCCURRENCES OF '9999-12-31' IN lv_json WITH space.

  CHECK lv_json IS NOT INITIAL.

  /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = gt_zppt0008_ecc ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0218
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0218 .

  CHECK p_218_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 40.
  DATA lv_field_s4 TYPE c LENGTH 40.

  PERFORM f_select_data_s4 USING 'ZSDT0218'.

  PERFORM f_get_table_info USING 'ZSDT0218' CHANGING lt_fields.

  SORT gt_zsdt0218_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0218_ecc.

  LOOP AT gt_zsdt0218_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0218'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-numeroreceita.
    <fs_alv>-chave_2 = <fs_ecc>-numeropedido.
    <fs_alv>-chave_3 = <fs_ecc>-cpfrt.
    <fs_alv>-chave_4 = <fs_ecc>-receitakey.

    READ TABLE gt_zsdt0218_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY numeroreceita = <fs_ecc>-numeroreceita
                 numeropedido = <fs_ecc>-numeropedido
                 cpfrt = <fs_ecc>-cpfrt
                 receitakey = <fs_ecc>-receitakey.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0219
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0219 .

  CHECK p_219_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 40.
  DATA lv_field_s4 TYPE c LENGTH 40.

  PERFORM f_select_data_s4 USING 'ZSDT0219'.

  PERFORM f_get_table_info USING 'ZSDT0219' CHANGING lt_fields.

  SORT gt_zsdt0219_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0219_ecc.

  LOOP AT gt_zsdt0219_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0219'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-numeroreceita.
    <fs_alv>-chave_2 = <fs_ecc>-numeropedido.
    <fs_alv>-chave_3 = <fs_ecc>-cpfrt.
    <fs_alv>-chave_4 = <fs_ecc>-codigomapa.
    <fs_alv>-chave_5 = <fs_ecc>-receitakey.
    <fs_alv>-chave_6 = <fs_ecc>-produto_id_agriq.

    READ TABLE gt_zsdt0219_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY numeroreceita = <fs_ecc>-numeroreceita
                 numeropedido = <fs_ecc>-numeropedido
                 cpfrt = <fs_ecc>-cpfrt
                 codigomapa = <fs_ecc>-codigomapa
                 receitakey = <fs_ecc>-receitakey
                 produto_id_agriq = <fs_ecc>-produto_id_agriq.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_inserir_individual
*&---------------------------------------------------------------------*
FORM f_inserir_individual .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0302
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0302 .

  CHECK p_302_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 40.
  DATA lv_field_s4 TYPE c LENGTH 40.

  PERFORM f_select_data_s4 USING 'ZSDT0302'.

  PERFORM f_get_table_info USING 'ZSDT0302' CHANGING lt_fields.

  SORT gt_zsdt0302_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0302_ecc.

  LOOP AT gt_zsdt0302_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0302'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_cgd.
    <fs_alv>-chave_2 = <fs_ecc>-ch_referencia.

    READ TABLE gt_zsdt0302_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_cgd = <fs_ecc>-nro_cgd
                 ch_referencia = <fs_ecc>-ch_referencia.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_alv_zsdt0298
*&---------------------------------------------------------------------*
FORM f_processa_alv_zsdt0298 .

  CHECK p_298_x6 IS NOT INITIAL.

  DATA lt_fields TYPE TABLE OF dbfield.

  DATA lv_field_ecc TYPE c LENGTH 40.
  DATA lv_field_s4 TYPE c LENGTH 40.

  PERFORM f_select_data_s4 USING 'ZSDT0298'.

  PERFORM f_get_table_info USING 'ZSDT0298' CHANGING lt_fields.

  SORT gt_zsdt0298_ecc.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0298_ecc.

  LOOP AT gt_zsdt0298_ecc ASSIGNING FIELD-SYMBOL(<fs_ecc>).

    DATA(lv_tabix) = sy-tabix.

    APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-ttl_col = lines( lt_fields ) - 1.
    <fs_alv>-tabix_ecc = lv_tabix.

    <fs_alv>-tcode = 'ZSDT0112'.
    <fs_alv>-ztable = 'ZSDT0298'.
    <fs_alv>-existe_ecc = abap_true.

    <fs_alv>-chave_1 = <fs_ecc>-nro_cgd.
    <fs_alv>-chave_2 = <fs_ecc>-ch_referencia.
    <fs_alv>-chave_3 = <fs_ecc>-id.

    READ TABLE gt_zsdt0298_s4 ASSIGNING FIELD-SYMBOL(<fs_s4>)
        WITH KEY nro_cgd = <fs_ecc>-nro_cgd
                 ch_referencia = <fs_ecc>-ch_referencia
                 id = <fs_ecc>-id.

    IF sy-subrc EQ 0.

      <fs_alv>-existe_s4 = abap_true.

      IF <fs_ecc> NE <fs_s4>.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

          CHECK <fs_fields>-name NE 'MANDT'.

          lv_field_ecc = '<FS_ECC>-' && <fs_fields>-name.
          lv_field_s4 = '<FS_S4>-' && <fs_fields>-name.

          ASSIGN (lv_field_ecc) TO FIELD-SYMBOL(<fs_col_ecc>).
          ASSIGN (lv_field_s4) TO FIELD-SYMBOL(<fs_col_s4>).

          CHECK <fs_col_ecc> IS ASSIGNED AND <fs_col_s4> IS ASSIGNED.

          IF <fs_col_ecc> <> <fs_col_s4>.

            <fs_alv>-tem_dif = abap_true.
            <fs_alv>-coluna_dif = <fs_fields>-name.
            <fs_alv>-valor_dif = <fs_col_s4>.
            ADD 1 TO <fs_alv>-qtde_dif.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF <fs_alv>-tem_dif = abap_true.

      <fs_alv>-icon = icon_red_light.
      <fs_alv>-msgtx = 'Verificar diferenças'.

      IF p_sobre IS NOT INITIAL.
        <fs_alv>-icon = icon_yellow_light.
      ENDIF.

    ELSEIF <fs_alv>-existe_s4 = abap_true.

      <fs_alv>-icon = icon_green_light.
      <fs_alv>-msgtx = 'Inserido corretamente'.

    ELSE.

      <fs_alv>-icon = icon_yellow_light.
      <fs_alv>-msgtx = 'Aguardando inserção'.

    ENDIF.

    <fs_alv>-registro = icon_table_settings.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0218
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0218 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0218 TYPE TABLE OF zsdt0218.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0218_ecc ASSIGNING FIELD-SYMBOL(<fs_0218>)
      WITH KEY numeroreceita = <fs_alv>-chave_1
               numeropedido = <fs_alv>-chave_2
               cpfrt = <fs_alv>-chave_3
               receitakey = <fs_alv>-chave_4.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0218> TO lt_zsdt0218.

  ENDLOOP.

  IF lt_zsdt0218[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0218' 50.

  MODIFY zsdt0218 FROM TABLE lt_zsdt0218.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0219
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0219 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0219 TYPE TABLE OF zsdt0219.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0219_ecc ASSIGNING FIELD-SYMBOL(<fs_0219>)
      WITH KEY numeroreceita = <fs_alv>-chave_1
               numeropedido = <fs_alv>-chave_2
               cpfrt = <fs_alv>-chave_3
               codigomapa = <fs_alv>-chave_4
               receitakey = <fs_alv>-chave_5
               produto_id_agriq = <fs_alv>-chave_6.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0219> TO lt_zsdt0219.

  ENDLOOP.

  IF lt_zsdt0219[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0219' 50.

  MODIFY zsdt0219 FROM TABLE lt_zsdt0219.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0302
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0302 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0302 TYPE TABLE OF zsdt0302.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0302_ecc ASSIGNING FIELD-SYMBOL(<fs_0302>)
      WITH KEY nro_cgd = <fs_alv>-chave_1
               ch_referencia = <fs_alv>-chave_2.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0302> TO lt_zsdt0302.

  ENDLOOP.

  IF lt_zsdt0302[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0302' 50.

  MODIFY zsdt0302 FROM TABLE lt_zsdt0302.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insere_zsdt0298
*&---------------------------------------------------------------------*
FORM f_insere_zsdt0298 .

  DATA lv_apaga TYPE c.

  DATA lt_zsdt0298 TYPE TABLE OF zsdt0298.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE icon = icon_yellow_light.

    READ TABLE gt_zsdt0298_ecc ASSIGNING FIELD-SYMBOL(<fs_0298>)
      WITH KEY nro_cgd = <fs_alv>-chave_1
               ch_referencia = <fs_alv>-chave_2
               id = <fs_alv>-chave_3.

    CHECK sy-subrc EQ 0.

    APPEND <fs_0298> TO lt_zsdt0298.

  ENDLOOP.

  IF lt_zsdt0298[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM f_sap_indicator USING 'Inserindo registros ZSDT0298' 50.

  MODIFY zsdt0298 FROM TABLE lt_zsdt0298.

  IF sy-subrc EQ 0.
    MESSAGE s016(ds) WITH 'Inserido com sucesso'.
  ELSE.
    MESSAGE s016(ds) WITH 'Erro ao inserir' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
