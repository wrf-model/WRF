pipeline {
  agent any
  stages {
    stage('pull the s3 libraries') {
      steps {
        sh '''curl -O https://s3.amazonaws.com/kelvin-softwares/wtf_v04.07_small.tar
curl -O https://s3.amazonaws.com/kelvin-softwares/classic_lib.zip'''
      }
    }
  }
}