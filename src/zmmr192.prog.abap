*&---------------------------------------------------------------------*
*& Report ZMMR192
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr192.

TYPE-POOLS: truxs.
PARAMETERS: p_file TYPE  rlgrap-filename.
TYPES: BEGIN OF ty_model,
         doc(30)  TYPE c, "Documento de Compra
         item(30) TYPE c, "Item
         mat(30)  TYPE c, "Metrial
         cent(30) TYPE c, "Centro
         ncm(30)  TYPE c, "Código NCM
       END OF ty_model,

       BEGIN OF  ty_ekpo,
         ebeln   TYPE ekpo-ebeln,
         matnr   TYPE ekpo-matnr,
         txz01   TYPE ekpo-txz01,
         loekz   TYPE ekpo-loekz,
         j_1bnbm TYPE ekpo-j_1bnbm,
       END OF ty_ekpo.

DATA: it_model       TYPE STANDARD TABLE OF ty_model,
      wa_model       TYPE ty_model,
      it_ekpo        TYPE TABLE OF ty_ekpo WITH HEADER LINE,
      wa_ekpo        TYPE ekpo,
      it_raw         TYPE truxs_t_text_data,
      it_poitem_ped  TYPE STANDARD TABLE OF bapimepoitem, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
      wa_poitemx_ped TYPE bapimepoitemx,
      it_return_ped  TYPE STANDARD TABLE OF bapiret2, "TABLE OF BAPIRET2 WITH HEADER LINE,
      wa_return_ped  TYPE bapiret2,
      txt_msg        TYPE text150,
      it_poitemx_ped TYPE STANDARD TABLE OF bapimepoitemx,
      it_popartner   TYPE STANDARD TABLE OF bapiekkop.

* At selection screen
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.
***********************************************************************
*START-OF-SELECTION.
START-OF-SELECTION.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = it_raw       " WORK TABLE
      i_filename           = p_file
    TABLES
      i_tab_converted_data = it_model[]    "ACTUAL DATA
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
***********************************************************************
* END-OF-SELECTION.
END-OF-SELECTION.
*  LOOP AT it_model INTO wa_model.
*    WRITE:/
*      wa_model-doc,
*      wa_model-item,
*      wa_model-mat,
*      wa_model-cent,
*      wa_model-ncm.
*  ENDLOOP.

  CHECK it_model[] IS NOT INITIAL.

  DELETE ADJACENT DUPLICATES FROM it_model[].


  DATA ebeln_range TYPE RANGE OF ebeln.

  ebeln_range = VALUE #( FOR ls_ebeln IN it_model[]
                               LET s = 'I'
                                   o = 'EQ'
                               IN sign   = s
                                  option = o
                             ( low = ls_ebeln )
                        ).
  SORT ebeln_range ASCENDING .

  DELETE ADJACENT DUPLICATES FROM ebeln_range.



  REFRESH it_ekpo.

  SELECT  DISTINCT ebeln matnr txz01 loekz
    INTO TABLE it_ekpo
    FROM ekpo
    WHERE ebeln IN ebeln_range
    ORDER BY ebeln.

*&-------------------------------------------------------------------------------------------------------------------
*&  "Alterar NCM tem pedido.
*&-------------------------------------------------------------------------------------------------------------------

  CHECK it_ekpo[] IS NOT INITIAL.

  "IF wa_zmmt0168-steuc NE ws_ekpo-j_1bnbm.

*      it_poitem_ped = VALUE #( ( po_item = it_ekpo-ebelp
*                                 bras_nbm = wa_zmmt0168-steuc ) ).
*
*      it_poitemx_ped = VALUE #( ( po_item = ws_ekpo-ebelp
*                                 bras_nbm = abap_true ) ).

  FREE: it_return_ped.
  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = it_ekpo-ebeln
    TABLES
      return        = it_return_ped
      poitem        = it_poitem_ped
      poitemx       = it_poitemx_ped
      popartner     = it_popartner.


  READ TABLE it_return_ped TRANSPORTING NO FIELDS WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    "     APPEND VALUE #( message_v1 = 'Pedido atualizado com sucesso' ) TO  gs_log.
  ENDIF.
  "ENDIF.

  txt_msg = |Foram excluidos o total de  registros!|.

  MESSAGE txt_msg TYPE 'I' DISPLAY LIKE 'S'.
