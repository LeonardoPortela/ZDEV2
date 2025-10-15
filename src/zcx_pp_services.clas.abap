CLASS zcx_pp_services DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF zcx_pp_services,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_pp_services .
    CONSTANTS:
      BEGIN OF planning_quantity_negative,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '038',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF planning_quantity_negative .
    CONSTANTS:
      BEGIN OF planning_quantity_unavailable,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF planning_quantity_unavailable .
    CONSTANTS:
      BEGIN OF planning_quantity_insufficient,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF planning_quantity_insufficient .
    CONSTANTS:
      BEGIN OF planning_not_found,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF planning_not_found .
    CONSTANTS:
      BEGIN OF process_order_not_released,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF process_order_not_released .
    CONSTANTS:
      BEGIN OF process_order_not_exist,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF process_order_not_exist .
    CONSTANTS:
      BEGIN OF oc_alreary_registred,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF oc_alreary_registred .
    CONSTANTS:
      BEGIN OF material_balance_insufficient,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF material_balance_insufficient .
    CONSTANTS:
      BEGIN OF order_already_concluded,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF order_already_concluded .
    CONSTANTS:
      BEGIN OF order_already_storned,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF order_already_storned .
    CONSTANTS:
      BEGIN OF ov_quantity_unavailable,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF ov_quantity_unavailable .
    CONSTANTS:
      BEGIN OF material_locked,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '033',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF material_locked .
    CONSTANTS:
      BEGIN OF lote_is_locked,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '034',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF lote_is_locked .
    CONSTANTS:
      BEGIN OF order_already_moviment,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '036',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF order_already_moviment .
    CONSTANTS:
      BEGIN OF quantity_invalid,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '037',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF quantity_invalid .
    CONSTANTS:
      BEGIN OF largest_quantity_prod ,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '041',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF largest_quantity_prod .
    CONSTANTS:
      BEGIN OF largest_quantity ,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '042',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF largest_quantity .

    CONSTANTS:
      BEGIN OF oc_invalid,
        msgid TYPE symsgid VALUE 'ZPPM001',
        msgno TYPE symsgno VALUE '040',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF oc_invalid.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_PP_SERVICES IMPLEMENTATION.


  METHOD CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
    CALL METHOD SUPER->CONSTRUCTOR
      EXPORTING
        PREVIOUS = PREVIOUS.
    CLEAR ME->TEXTID.
    IF TEXTID IS INITIAL.
      IF_T100_MESSAGE~T100KEY = ZCX_PP_SERVICES .
    ELSE.
      IF_T100_MESSAGE~T100KEY = TEXTID.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
