**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br )                  |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Report de Execução de Notas gerado apartir do Mobile                      |*
**/===========================================================================\*

REPORT ZPMR0046.

DATA T_ZPMT0019 TYPE ZPMT0019_T.

DATA(OBJ_CREATE) = NEW ZCL_PM_ORDEM( ).

START-OF-SELECTION.

  SELECT *
    FROM ZPMT0014
    INTO TABLE @DATA(T_ZPMT0014)
    WHERE STATP EQ @ABAP_FALSE.

  CHECK SY-SUBRC IS INITIAL.

  SELECT *
    FROM ZPMT0019
    INTO TABLE T_ZPMT0019
    FOR ALL ENTRIES IN T_ZPMT0014
    WHERE IDNOT EQ T_ZPMT0014-IDNOT.

  LOOP AT T_ZPMT0014 INTO DATA(W_NOTAS).

    CALL METHOD OBJ_CREATE->CRIAR_NOTAS
      EXPORTING
        I_NOTAS  = W_NOTAS       " Estrutura Notas
        T_CAUSAS = T_ZPMT0019    " Tabela de causas
      IMPORTING
        E_NOTA   = DATA(_NOTA)   " Retorno da Nota
        E_MSG    = DATA(_MSG).   " Retorno da mensagen de Erro/Sucesso

    IF _NOTA IS NOT INITIAL.
*     "// Preenche a Nota com ZEROS
      W_NOTAS-QMNUM = |{ _NOTA ALPHA = IN }|.
*     "// Autera o Status do Processamento para "P" de Processado
      W_NOTAS-STATP = 'P'.  "// Processado
    ELSE.
*     "// Autera o Status do Processamento para "E" de Erro
      W_NOTAS-STATP = 'E'.  "// Erro no Processamento
    ENDIF.

*     "// Aplica na Tabela Fisica
    MODIFY ZPMT0014 FROM W_NOTAS.
    COMMIT WORK.

  ENDLOOP.
