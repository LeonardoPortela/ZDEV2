SELECTION-SCREEN BEGIN OF BLOCK s01 WITH FRAME TITLE text-s01."text-s01: Balancete contábil para Consolidação

SELECT-OPTIONS: s_bukrs FOR t001-bukrs NO INTERVALS OBLIGATORY.

PARAMETERS: p_bukrs TYPE bukrs NO-DISPLAY, "Empresa
            p_year  TYPE gjahr OBLIGATORY, "Ano
            p_month TYPE month.            "Mês

PARAMETERS: p_f01   TYPE xflag NO-DISPLAY, "Balancete(F.01)
            p_gl035 AS CHECKBOX DEFAULT 'X'. "Balancete(ZGL035)

SELECTION-SCREEN END OF BLOCK s01.
