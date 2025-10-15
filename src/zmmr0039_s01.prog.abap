SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE text-s01. "text-s01: "Parâmetros de execução

PARAMETERS: p_type TYPE ztp_estrat OBLIGATORY,  "Tipo de Estratégia
            p_date TYPE erdat DEFAULT sy-datum. "Data base

SELECTION-SCREEN END OF BLOCK s1.
