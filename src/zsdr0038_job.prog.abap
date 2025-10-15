************************************************************************************
*&                        AMAGGI                                                  &*
*&--------------------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                              &*
*& Autor....: Jaime Tassoni                                                       &*
*& Data.....: 08.09.2025                                                          &*
*& Descrição: Efetua Distribuicao Insumos Sementes                                &*
************************************************************************************
REPORT zsdr0038_job.

******************************************************
* cariaveis
******************************************************
DATA: lc_zsds093 TYPE zsds093,
      lc_zsds094 TYPE zsds094_tt.

******************************************************
* parametros entrada
******************************************************
PARAMETERS: p_zsd093 TYPE string,
            p_zsd094 TYPE string.

******************************************************
* start
******************************************************

*-json
/ui2/cl_json=>deserialize( EXPORTING json = p_zsd093 CHANGING data = lc_zsds093 ).
/ui2/cl_json=>deserialize( EXPORTING json = p_zsd094 CHANGING data = lc_zsds094 ).

*-executa processamento
CALL FUNCTION 'ZSD_INSUMOS_DISTRIBUICAO'
  EXPORTING
    i_zsds093 = lc_zsds093
  TABLES
    t_zsds094 = lc_zsds094.

******************************************************
******************************************************
