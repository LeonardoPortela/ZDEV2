CLASS zcl_im__atual_ordem_mobman DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_workorder_update .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IM__ATUAL_ORDEM_MOBMAN IMPLEMENTATION.


  METHOD if_ex_workorder_update~archive_objects.
  ENDMETHOD.


  METHOD if_ex_workorder_update~at_deletion_from_database.
  ENDMETHOD.


  METHOD if_ex_workorder_update~at_release.
  ENDMETHOD.


  METHOD if_ex_workorder_update~at_save.
  ENDMETHOD.


  METHOD if_ex_workorder_update~before_update.

    DATA: lt_riwol      TYPE TABLE OF riwol,
          lt_riwol_upd  TYPE TABLE OF riwol,
          lt_ripwo_ex   TYPE TABLE OF ripw0,
          lt_briwol_mem TYPE TABLE OF riwol,
          lt_iser02     TYPE TABLE OF rserxx,
          lt_list       TYPE TABLE OF bapi_alm_order_objectlist,
          lt_return     TYPE TABLE OF bapiret2,
          lv_tabix      TYPE sy-tabix.


    IF sy-tcode EQ 'IW31' OR
       sy-tcode EQ 'IW32' OR
       sy-tcode EQ 'IW34' OR
       sy-tcode EQ 'IW21' OR  "137558 Criação de ordem a partir da nota não está disparando JSON para o app (IW34) - PSA
       sy-tcode EQ 'IW22'.    "137558 Criação de ordem a partir da nota não está disparando JSON para o app (IW34) - PSA


      READ TABLE it_header ASSIGNING FIELD-SYMBOL(<fs_header>) INDEX 1.

      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
        EXPORTING
          number   = <fs_header>-aufnr
        TABLES
          et_olist = lt_list
          return   = lt_return.


      CALL FUNCTION 'IWOL_GET_OBJECT_LIST_ALL'
        EXPORTING
          i_aufnr        = <fs_header>-aufnr
        TABLES
          iriwol         = lt_riwol
          iriwol_upd     = lt_riwol_upd
          iripw0_exp     = lt_ripwo_ex
        EXCEPTIONS
          no_object_list = 1
          no_order       = 2
          OTHERS         = 3.
      IF sy-subrc = 0.
        DELETE lt_riwol WHERE ihnum IS INITIAL.
        SORT lt_riwol_upd BY ihnum dbknz.
        LOOP AT lt_riwol ASSIGNING FIELD-SYMBOL(<fs_riwol>).
          lv_tabix = sy-tabix.

          READ TABLE lt_riwol_upd ASSIGNING FIELD-SYMBOL(<fs_riwol_upd>)
          WITH KEY ihnum = <fs_riwol>-ihnum
                   dbknz = 'D'
                   BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            DELETE lt_riwol INDEX lv_tabix.
          ENDIF.

        ENDLOOP.

      ENDIF.


      MESSAGE ID 'IW' TYPE 'S' NUMBER '080' WITH <fs_header>-aufnr.


      CALL FUNCTION 'ZPM_DISPARA_API_ORDEM' IN BACKGROUND TASK AS SEPARATE UNIT
        EXPORTING
          header    = it_header
          operation = it_operation
        TABLES
          t_list    = lt_list
          t_riwol   = lt_riwol.

    ENDIF.

  ENDMETHOD.


  METHOD if_ex_workorder_update~cmts_check.
  ENDMETHOD.


  METHOD if_ex_workorder_update~initialize.
  ENDMETHOD.


  METHOD if_ex_workorder_update~in_update.

*    DATA: lt_riwol      TYPE TABLE OF riwol,
*          lt_riwol_upd  TYPE TABLE OF riwol,
*          lt_ripwo_ex   TYPE TABLE OF ripw0,
*          lt_briwol_mem TYPE TABLE OF riwol,
*          lt_iser02     TYPE TABLE OF rserxx,
*          lt_list       TYPE TABLE OF bapi_alm_order_objectlist,
*          lt_return     TYPE TABLE OF bapiret2.
*
*    IF sy-tcode EQ 'IW31' OR
*       sy-tcode EQ 'IW32' .
*
*
*      READ TABLE it_header ASSIGNING FIELD-SYMBOL(<fs_header>) INDEX 1.
*
*      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
*        EXPORTING
*          number   = <fs_header>-aufnr
*        TABLES
*          et_olist = lt_list
*          return   = lt_return.
*
*      CALL FUNCTION 'IWOL_GET_OBJECT_LIST_ALL'
*        EXPORTING
*          i_aufnr        = <fs_header>-aufnr
*        TABLES
*          iriwol         = lt_riwol
*          iriwol_upd     = lt_riwol_upd
*          iripw0_exp     = lt_ripwo_ex
*        EXCEPTIONS
*          no_object_list = 1
*          no_order       = 2
*          OTHERS         = 3.
*      IF sy-subrc = 0.
*
*      ENDIF.
*
*      CALL FUNCTION 'ZPM_DISPARA_API_ORDEM' IN BACKGROUND TASK AS SEPARATE UNIT
*        EXPORTING
*          header    = it_header
*          operation = it_operation
*        TABLES
**          t_objk    = lt_objk
*          t_riwol   = lt_riwol.
*
*    ENDIF.

  ENDMETHOD.


  METHOD if_ex_workorder_update~number_switch.
  ENDMETHOD.


  METHOD if_ex_workorder_update~reorg_status_activate.
  ENDMETHOD.


  METHOD if_ex_workorder_update~reorg_status_act_check.
  ENDMETHOD.


  METHOD if_ex_workorder_update~reorg_status_revoke.
  ENDMETHOD.
ENDCLASS.
