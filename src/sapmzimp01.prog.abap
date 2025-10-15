************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 05.05.2009                                          *
* Tipo de prg ...: Pool de módulos                                     *
* Objetivo    ...: Lançamento/Modificação/Exibição de Tributos a Pagar *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 05.05.2009    Desenvolvedor ABAP   Criação              DEVK905828   *
************************************************************************


INCLUDE MZIMP01TOP                              .    " global Data

* INCLUDE MZIMP01O01                              .  " PBO-Modules
* INCLUDE MZIMP01I01                              .  " PAI-Modules
* INCLUDE MZIMP01F01                              .  " FORM-Routines

INCLUDE MZIMP01_CONFIG_TELAO01.

INCLUDE MZIMP01_USER_COMMAND_0001I01.

INCLUDE MZIMP01_ZF_VERIF_SKATF01.
