"Name: \FU:J_1B_NF_DOCUMENT_CANCEL\SE:END\EI
ENHANCEMENT 0 ZJ_1B_NF_DOCUMENT_CANCEL.

IF NOT doc_number       IS INITIAL AND
   NOT wk_header-docref IS INITIAL AND
       wk_header-manual EQ abap_true.

  IF doc_number NE wk_header-docref.

    SELECT SINGLE *
      FROM zsdt_export INTO @DATA(_export)
     WHERE docnum EQ @wk_header-docref.

    IF ( sy-subrc = 0 ).
* Atualização das tabelas de reversão (estorno) da NF Retorno
      zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->atualizar_tabelas_z( EXPORTING i_docnum = wk_header-docref ).

    ENDIF.

  ENDIF.

ENDIF.

ENDENHANCEMENT.
