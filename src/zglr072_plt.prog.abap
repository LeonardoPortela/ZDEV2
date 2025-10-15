*&---------------------------------------------------------------------*
*&  Include           ZGLR072_PLT
*& LISTA DE TIPO DE RELATORIO
*&---------------------------------------------------------------------*
INITIALIZATION.

  PERFORM feed_list.

form  feed_list.

  DATA: name_list TYPE vrm_id,
        values    TYPE vrm_values,
        value     TYPE  vrm_value.

  name_list = 'P_LBTIPO'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = name_list
      values          = VALUE vrm_values( ( key = '1' text = 'Analitico' )
                                          ( key = '2' text = 'Sintético' )
                                          ( key = '3' text = 'Conciliação' )
*Inicio Alteração - Leandro Valentim ferreira - 10.08.23 - #119411
                                          ( key = '4' text = 'Resultado' ) )
*Fim Alteração - Leandro Valentim ferreira - 10.08.23 - #119411
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

ENDFORM.
