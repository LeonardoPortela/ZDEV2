FUNCTION Z_CONVERTE_CARACTERES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_STRING) TYPE  MSGTX OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_STRING) TYPE  MSGTX
*"----------------------------------------------------------------------

DATA: lt_mapping TYPE TABLE OF string,  " Tabela para pares de mapeamento
        wa_mapping TYPE string,           " Linha da tabela de mapeamento
        lv_special TYPE string,           " Caractere especial
        lv_normal TYPE string.            " Caractere normal correspondente

  " Populando tabela de mapeamento com pares de substituição
  APPEND 'Ç|C' TO lt_mapping.
  APPEND 'Á|A' TO lt_mapping.
  APPEND 'É|E' TO lt_mapping.
  APPEND 'Í|I' TO lt_mapping.
  APPEND 'Ó|O' TO lt_mapping.
  APPEND 'Ú|U' TO lt_mapping.
  APPEND 'Ã|A' TO lt_mapping.
  APPEND 'Õ|O' TO lt_mapping.
  APPEND 'Â|A' TO lt_mapping.
  APPEND 'Ê|E' TO lt_mapping.
  APPEND 'Î|I' TO lt_mapping.
  APPEND 'Ô|O' TO lt_mapping.
  APPEND 'Û|U' TO lt_mapping.

  " Inicializando string normalizada com o valor da entrada
  EV_STRING = IV_STRING.
  TRANSLATE EV_STRING TO UPPER CASE.

  " Loop para substituir caracteres especiais
  LOOP AT lt_mapping INTO wa_mapping.
    " Separar o par de mapeamento em caractere especial e caractere normal
    SPLIT wa_mapping AT '|' INTO lv_special lv_normal.
    " Substituir todas as ocorrências do caractere especial na string
    REPLACE ALL OCCURRENCES OF lv_special IN EV_STRING WITH lv_normal.
  ENDLOOP.



ENDFUNCTION.
