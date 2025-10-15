@AbapCatalog.sqlViewName: 'ZIINZSDT51'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@ClientHandling.type: #CLIENT_DEPENDENT
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Teste CDC Simples'
@Analytics: {
    dataCategory: #FACT,
    dataExtraction: {
    enabled: true,

    delta.changeDataCapture: {

    mapping: [{
                role: #MAIN,
                table: 'ZSDT0051',
                viewElement: [ 'Simulador' ],
                tableElement: [ 'NRO_SOL_OV' ]
               }
             ]
                             }
                    }
            }
define view ZI_IN_ZSDT0051
  as select from zsdt0051
{
  key nro_sol_ov as Simulador,
      dtde_logist,
      vbeln
}
