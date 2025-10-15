*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| Este programa tem como objetivo a comunicação com WebService da Tipcard   |*
*| sera executado diariamente conforme o tempo do JOB                        |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|  Tester/Funcional:                                                        |*
*|    + Leila Mara Vançan ( leila.vancan@grupomaggi.com.br )                 |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*

REPORT  ZLESJ0008.

**********************************************************************
* Variaveis
**********************************************************************
DATA: VAR_MSG   TYPE STRING.

**********************************************************************
* Classes que serão utilizadas nesse processo.
**********************************************************************
DATA: OBJ_ZCL_WEBSERVICE_TIPCARD TYPE REF TO ZCL_WEBSERVICE_TIPCARD.

**********************************************************************
* Classe Exception
**********************************************************************
DATA: CX_EXCEPTION TYPE REF TO ZCX_WEBSERVICE.

**********************************************************************
* Inicio do Programa MAIN
**********************************************************************
FREE: OBJ_ZCL_WEBSERVICE_TIPCARD.

CREATE OBJECT OBJ_ZCL_WEBSERVICE_TIPCARD.

TRY.

    SELECT * INTO TABLE @DATA(IT_ZLEST0159)
      FROM ZLEST0159.

    LOOP AT IT_ZLEST0159 INTO DATA(WA_ZLEST0159).
      OBJ_ZCL_WEBSERVICE_TIPCARD->ATUALIZAR_VALORES( I_GRUPO = WA_ZLEST0159-DS_GRUPO ).
      OBJ_ZCL_WEBSERVICE_TIPCARD->BUSCAR_ROTAS( I_GRUPO = WA_ZLEST0159-DS_GRUPO ).
    ENDLOOP.

  CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
    VAR_MSG = CX_EXCEPTION->GET_TEXT( ).
    MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH VAR_MSG.
ENDTRY.
