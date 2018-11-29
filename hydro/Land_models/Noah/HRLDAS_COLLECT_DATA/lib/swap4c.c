/*
  Program Name:
  Author(s)/Contact(s):
  Abstract:
  History Log:
 
  Usage:
  Parameters: <Specify typical arguments passed>
  Input Files:
        <list file names and briefly describe the data they include>
  Output Files:
        <list file names and briefly describe the information they include>
 
  Condition codes:
        <list exit condition or error codes returned >
        If appropriate, descriptive troubleshooting instructions or
        likely causes for failures could be mentioned here with the
        appropriate error code
 
  User controllable options: <if applicable>
 
*/
void swap4c(char i[], int n) {
  int j;
  int m;
  char hold;

  for (m=0; m<n; m+=4){
    hold =i[m+0];
    i[m+0]=i[m+3];
    i[m+3]=hold;

    hold =i[m+1];
    i[m+1]=i[m+2];
    i[m+2]=hold;
    
  }
}
