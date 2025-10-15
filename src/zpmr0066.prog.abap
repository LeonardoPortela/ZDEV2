************************************************************************
*              ******************************************              *
*              *                 AMAGGI                 *              *
*              *       CONFIDENCIAL E PROPRIETÁRIO      *              *
*              *      TODOS OS DIREITOS RESERVADOS      *              *
*              ******************************************              *
************************************************************************
* Projeto       : Projeto Controle Ferramentaria                       *
* Objetivo      : Emprestimo de Materiais                              *
* Analista      : Alexandre Suzan WAGON                                *
* Desenvolvedor : Alexandre Suzan WAGON                                *
* Data          : 26/11/2020                                           *
* Transação     : ZPMR0066                                             *
* Observação    :                                                      *
*                                                                      *
*----------------------------------------------------------------------*

INCLUDE mzpmr0066top                            .
INCLUDE mzpmr0066i01                            .
INCLUDE mzpmr0066o01                            .
INCLUDE mzpmr0066form                           .



*FORM z_descarte_ext TABLES it_msg STRUCTURE bdcmsgcoll
*                                   USING lva_werks       TYPE zpmt0044-werks
*                                         lva_matnr       TYPE zpmt0044-matnr
*                                         lva_qnt         TYPE char1
*                                         lva_equnr       TYPE zpmt0044-equnr
*                                         lva_motv_desc   TYPE zpmt0044-observ.
*
*  PERFORM z_descarte TABLES it_msg
*               USING lva_werks
*                     lva_matnr
*                     lva_qnt
*                     lva_equnr
*                     lva_motv_desc.
*ENDFORM.
