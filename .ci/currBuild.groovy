void setBuildStatus(String message, String state) {
  step([
      $class: "GitHubCommitStatusSetter",
      reposSource: [$class: "ManuallyEnteredRepositorySource", url: "https://github.com/scala-computing/WRF_DOCKER"],
      contextSource: [$class: "ManuallyEnteredCommitContextSource", context: "ci/jenkins/build-status"],
      errorHandlers: [[$class: "ChangingBuildStatusErrorHandler", result: "UNUNSTABLE"]],
      statusResultSource: [ $class: "ConditionalStatusResultSource", results: [[$class: "AnyBuildResult", message: message, state: state]] ]
  ]);
}

def Instanceflag() {
    instanceId="""
    aws ec2 describe-instances --query 'Reservations[*].Instances[*].[InstanceId]' --filters Name=instance-state-name,Values=running  "Name=tag:Name,Values=wrf-test" --region us-east-1
    """
    instance=sh(script: instanceId, returnStdout: true)
    def running
    if(instance.size()<=3){
        running=false
    }else{
        running=true
    }
    return running
}

def is_build() {
   String jsonString = payload
             JsonSlurper slurper = new JsonSlurper()
             Map parsedJson = slurper.parseText(jsonString)
             def changed_list = parsedJson.get("commits").modified
             print(changed_list[0].size())         
             def file_status
    if(changed_list[0].size() == 1 && changed_list[0][0]=="README.MD" ){
        file_status=false
    }else{
        file_status=true
    }
    return file_status
}

node {
      sh '''ls -ll
      echo $payload > sample.json'''
      branch = sh (
                 script: 'cat sample.json | jq .ref | awk -F\'/\' \'{print $3}\'|awk -F\'"\' \'{print $1}\'' ,
                 returnStdout: true
             ).trim()
     if(is_build()=true){      
      try{
         stage('Terrafrom stuff'){
             def dirpath = "/var/lib/jenkins/workspace/wrf_test_case/test"
             dir(dirpath){
                   sh 'sudo terraform init'
                   sh 'sudo terraform plan'
                   sh 'sudo terraform apply -auto-approve' 
              }
               
          }
      }catch (e) {
       // If there was an exception thrown, the build failed
       currentBuild.result = "FAILED"
       setBuildStatus("Build succeeded", "FAILURE");
       throw e
      }finally {
      // Success or failure, always send notifications
       stage('check instance running status')
        {
          script{
            while(Instanceflag()==true){
            def flag=Instanceflag()
            if(flag==true){
            print("Instances are still running")
            }else{
              print("Instances are stopped")
              ///
              print(flag)
              break
              }
             }
            }
        }
        
     stage ('Download Output')
     {
          def dirpath = "/var/lib/jenkins/workspace/wrf_test_case/"
             dir(dirpath){
                sh 'rm -rf output_testcase' 
                sh 'rm -rf wrf_output.zip'
                sh 'mkdir output_testcase' 
                sh 'aws s3 cp s3://wrf-testcase/output/ output_testcase/ --region us-east-1 --recursive'
                sh 'zip -r wrf_output.zip output_testcase'
             }
         
     }
     stage('Email')
        {
            emailext attachmentsPattern: 'wrf_output.zip', 
            body: 'Please fined the attachment of the output', 
            subject: currentBuild.currentResult + " : " + env.JOB_NAME, 
            to: 'kwoo@scalacomputing.com'
        }    
        
        }
     } else {
        stage('Email')
        {
            body: 'There is no changes to build this jon', 
            subject: currentBuild.currentResult + " : " + env.JOB_NAME, 
            to: 'kwoo@scalacomputing.com'
        }   
     }   
}