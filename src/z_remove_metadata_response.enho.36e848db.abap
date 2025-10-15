"Name: \TY:/IWCOR/CL_DS_EP_WRITER_JSON\ME:WRITE_ENTITY_INTERNAL\SE:BEGIN\EI
ENHANCEMENT 0 Z_REMOVE_METADATA_RESPONSE.
*  "//Autor....: Enio Jesus
*  "//Descrição: Implementação para não incluir os metadatados no response do json em uma requisição.
*  "//Data.....: 08.09.2016
*
*  write_entity_properties(
*    EXPORTING
*      it_property = is_entity_info-properties
*      iv_keys     = is_entity_info-number_of_keys
*      is_data     = is_data
*    IMPORTING
*      ev_data     = data(json_properties)
*   ).
*
*  " write entry
*  shift json_properties left deleting leading ','.
*  CONCATENATE ev_entry iv_separator '{' json_properties '}' INTO ev_entry.
*
*  return.

ENDENHANCEMENT.
