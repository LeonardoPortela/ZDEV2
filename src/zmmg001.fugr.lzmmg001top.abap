*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: Maggi                                                   *
* Autor......: Daniela Machado                                         *
* Data.......: 21.07.2010                                              *
* Descrição  : Criação / Cancelamento da MIGO no SAP via SIGAM         *
* Projeto....: Maggi - Projeto Evoluir                                 *
* Cód Espec..: GAP_MM02                                                *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*
FUNCTION-POOL zmmg001.                      "MESSAGE-ID ..

************************************************************************
* Tabelas Internas
************************************************************************
*DATA: BEGIN OF it_log OCCURS 0.
*        INCLUDE STRUCTURE zfie_ret_document.
*DATA: END OF it_log.

DATA: it_zmmt_ee_zgr  TYPE TABLE OF zmmt_ee_zgr,
      it_zmmt_eee_zgr TYPE TABLE OF zmmt_eee_zgr,
      it_item         TYPE TABLE OF bapi2017_gm_item_create,
      it_serialnumber TYPE TABLE OF bapi2017_gm_serialnumber,
      it_return       TYPE TABLE OF bapiret2,
      it_outreturn    TYPE TABLE OF zfie_ret_document.

************************************************************************
* Estruturas
************************************************************************
DATA: wa_mov_estq     TYPE zmms001,
      wa_estorno      TYPE zmms003,
      wa_header       TYPE bapi2017_gm_head_01,
      wa_item         TYPE bapi2017_gm_item_create,
*      wa_mat_doc      TYPE bapi2017_gm_head_ret-mat_doc,
*      wa_doc_year     TYPE bapi2017_gm_head_ret-doc_year,
      wa_mat_doc2     TYPE bapi2017_gm_head_02,
      wa_head_ret     TYPE bapi2017_gm_head_ret,
      wa_serialnumber TYPE bapi2017_gm_serialnumber,
      wa_return       TYPE bapiret2,
      wa_outreturn    TYPE zfie_ret_document.

************************************************************************
* Constantes
************************************************************************
CONSTANTS: c_s(1) TYPE c                            VALUE 'S',
           c_i    TYPE zmms001-zrg_atlz             VALUE 'I',
           c_11   TYPE zfie_ret_document-interface  VALUE '11',
           c_mm   TYPE zfie_ret_document-id         VALUE 'MM',
           c_899  TYPE zfie_ret_document-num        VALUE '899',
           c_01   TYPE bapi2017_gm_code             VALUE '01'.

************************************************************************
* Variáveis
************************************************************
DATA: vg_gmcode  TYPE bapi2017_gm_code,
      vg_obj_key TYPE zmmt_ee_zgr-obj_key,
      vg_index   TYPE sy-tabix.
**&---------------------------------------------------------------------*
**&      Form  FM_SELECIONA_DADOS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM fm_seleciona_dados .
*  REFRESH: it_ausp, it_ausp_2, it_ausp_3, it_ausp_4.
*
*  IF s_centro EQ 'X'.
*
*    SELECT *
*      FROM t001w
*      INTO TABLE it_t001w
*    WHERE werks IN s_werks.
*
*    "Pedidos
*    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*      EXPORTING
*        input  = 'ZMMCCUSTOPO'
*      IMPORTING
*        output = vg_atinn.
*
*    CALL FUNCTION 'CLSE_SELECT_AUSP'
*      EXPORTING
*        klart                     = '032'
*        atinn                     = vg_atinn
*      TABLES
*        t_ausp                    = it_ausp_2
*      EXCEPTIONS
*        no_entry_found            = 1
*        parameters_not_sufficient = 2
*        OTHERS                    = 3.
*
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    "Centro
*    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*      EXPORTING
*        input  = 'ZMMCENTRO'
*      IMPORTING
*        output = vg_atinn.
*
*    CALL FUNCTION 'CLSE_SELECT_AUSP'
*      EXPORTING
*        klart                     = '032'
*        atinn                     = vg_atinn
*      TABLES
*        t_ausp                    = it_ausp_3
*      EXCEPTIONS
*        no_entry_found            = 1
*        parameters_not_sufficient = 2
*        OTHERS                    = 3.
*
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    "Tipos
*    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*      EXPORTING
*        input  = 'ZMMTIPODOC'
*      IMPORTING
*        output = vg_atinn.
*
*    CALL FUNCTION 'CLSE_SELECT_AUSP'
*      EXPORTING
*        klart                     = '032'
*        atinn                     = vg_atinn
*      TABLES
*        t_ausp                    = it_ausp_4
*      EXCEPTIONS
*        no_entry_found            = 1
*        parameters_not_sufficient = 2
*        OTHERS                    = 3.
*
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*  ELSE.
*
*    SELECT *
*           FROM csks
*           INTO TABLE it_csks
*           WHERE kokrs IN s_kokrs
*             AND bukrs IN s_bukrs
*             AND kostl IN s_kostl
*             AND datbi GE sy-datum
*             AND gsber IN s_werks
*             AND bukrs IN s_bukrs
*        ORDER BY kostl.
*
*    SELECT *
*      FROM t001
*      INTO TABLE it_t001
*      FOR ALL ENTRIES IN it_csks
*    WHERE bukrs EQ it_csks-bukrs.
*
*
*    SELECT *
*      FROM t001w
*      INTO TABLE it_t001w
*      FOR ALL ENTRIES IN it_csks
*    WHERE werks EQ it_csks-gsber.
*
*
*    SELECT *
*      INTO TABLE it_cskt
*      FROM cskt
*       FOR ALL ENTRIES IN it_csks
*     WHERE kokrs EQ it_csks-kokrs
*       AND kostl EQ it_csks-kostl
*    AND datbi EQ it_csks-datbi.
*
*    SELECT *
*      FROM zmmt0003
*      INTO TABLE it_zmmt0003
*      FOR ALL ENTRIES IN it_csks
*    WHERE kostl EQ it_csks-kostl
*    AND   dt_val_de  LE sy-datum
*    AND   dt_val_ate GE sy-datum.
*
*    "Requisiçoes
*    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*      EXPORTING
*        input  = 'ZMMCCUSTO'
*      IMPORTING
*        output = vg_atinn.
*
*    CALL FUNCTION 'CLSE_SELECT_AUSP'
*      EXPORTING
*        klart                     = '032'
*        atinn                     = vg_atinn
*      TABLES
*        t_ausp                    = it_ausp
*      EXCEPTIONS
*        no_entry_found            = 1
*        parameters_not_sufficient = 2
*        OTHERS                    = 3.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    "Pedidos
*    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*      EXPORTING
*        input  = 'ZMMCCUSTOPO'
*      IMPORTING
*        output = vg_atinn.
*
*    CALL FUNCTION 'CLSE_SELECT_AUSP'
*      EXPORTING
*        klart                     = '032'
*        atinn                     = vg_atinn
*      TABLES
*        t_ausp                    = it_ausp_2
*      EXCEPTIONS
*        no_entry_found            = 1
*        parameters_not_sufficient = 2
*        OTHERS                    = 3.
*
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*  ENDIF.
*
********  CLEAR it_ausp_aux[].
********  MOVE it_ausp[]   TO it_ausp_aux[].
********  DELETE ADJACENT DUPLICATES FROM it_ausp_aux COMPARING objek.
********
********  LOOP AT it_ausp_aux.
********    it_aux-objek = it_ausp_aux-objek.
********    it_aux-frggr = it_ausp_aux-objek(2).
********    it_aux-frgsx = it_ausp_aux-objek+2(2).
********    APPEND it_aux.
********  ENDLOOP.
********
********  CLEAR it_ausp_aux[].
********  MOVE it_ausp_2[] TO it_ausp_aux[].
********  DELETE ADJACENT DUPLICATES FROM it_ausp_aux COMPARING objek.
********
********  LOOP AT it_ausp_aux.
********    it_aux-objek = it_ausp_aux-objek.
********    it_aux-frggr = it_ausp_aux-objek(2).
********    it_aux-frgsx = it_ausp_aux-objek+2(2).
********    APPEND it_aux.
********  ENDLOOP.
********
********  DELETE ADJACENT DUPLICATES FROM it_aux COMPARING ALL FIELDS.
*
*  SELECT *
*     FROM t16fs
*  INTO TABLE it_t16fs.
*
*  SORT it_t16fs BY frggr frgsx.
*
*  SELECT *
*    FROM t16ft
*    INTO TABLE it_t16ft
*  WHERE spras EQ 'P'.
*  SORT it_t16ft BY frggr frgsx.
*
*  SELECT *
*    FROM t16fc
*  INTO TABLE it_t16fc.
*  SORT it_t16fc BY frggr frgco.
*
*  SELECT *
*    FROM t16fd
*  INTO TABLE it_t16fd.
*  SORT it_t16fd BY frggr frgco.
*
*  IF ( it_zmmt0003[] IS NOT INITIAL ).
*
*    SELECT *
*      FROM user_addr
*      INTO TABLE it_user_addr
*      FOR ALL ENTRIES IN it_zmmt0003[]
*      WHERE bname = it_zmmt0003-uname.
*    SORT it_user_addr BY name_textc bname.
*
*  ENDIF.
*ENDFORM.
