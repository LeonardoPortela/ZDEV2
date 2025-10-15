CLASS lcl_document_selector DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_documents_for_change
      RETURNING VALUE(rv_documents_for_change) TYPE zttfi_change_document_yt0049.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS set_data.

    DATA: gt_documents_for_change TYPE zttfi_change_document_yt0049.

ENDCLASS.

CLASS lcl_document_changer DEFINITION.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_document_selector TYPE REF TO lcl_document_selector.

    METHODS change_values.

    METHODS save_values.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: go_document_selector    TYPE REF TO lcl_document_selector,
          gt_documents_for_change TYPE zttfi_change_document_yt0049.

ENDCLASS.
