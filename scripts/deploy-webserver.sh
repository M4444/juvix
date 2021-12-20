#!/bin/sh

instanceId=$(aws ec2 run-instances \
  --image-id ami-04dd4500af104442f \
  --key-name juvix-server-builder \
  --instance-type c5.xlarge \
  --security-group-ids sg-0c97950cc2ee94d13 \
  --iam-instance-profile "Arn=arn:aws:iam::965844283396:instance-profile/juvix-server-ec2-builder" \
  --block-device-mappings DeviceName=/dev/sda1,Ebs="{VolumeSize=100,VolumeType=gp3}" \
  --tag-specifications ResourceType=instance,Tags="[{Key='Name',Value='Juvix Webserver Builder'},{Key='Project',Value='juvix-webserver'},{Key='CostCenter',Value='juvix'}]" \
  --instance-market-options MarketType=spot,SpotOptions="{SpotInstanceType=one-time,InstanceInterruptionBehavior=terminate,MaxPrice=0.085}" \
  --region eu-west-1 | jq -r .Instances[0].InstanceId)

echo "wait for builder to start..."

aws ec2 wait instance-status-ok --instance-ids "$instanceId" --region eu-west-1

commandId=$(aws ssm send-command \
    --document-name "AWS-RunShellScript" \
    --parameters "commands=[\"runuser -l  ec2-user -c 'aws s3 cp s3://juvix-webserver-configurations/build.sh ~/build.sh && chmod +x ~/build.sh && ~/build.sh'\"]" \
    --instance-ids "$instanceId" \
    --comment "Build and deploy juvix-webserver" \
    --timeout-seconds 1800 \
    --region eu-west-1 | jq -r .Command.CommandId)

echo "started build script ($commandId)!"

status=$(aws ssm get-command-invocation --command-id $commandId --instance-id $instanceId --region eu-west-1 --no-paginate | jq -r .Status)

while [[ "$status" != "Success" && "$status" != "Failed" ]]
do
    echo "waiting for build to finish..."
    sleep 10
    status=$(aws ssm get-command-invocation --command-id $commandId --instance-id $instanceId --region eu-west-1 --no-paginate | jq -r .Status)
done

if [[ "$status" == "Success" ]]; then
    echo "Build successful!"
    aws ec2 terminate-instances --instance-ids $instanceId --region eu-west-1 --no-paginate
    exit 0
else
    echo "Build failed!"
    aws ec2 terminate-instances --instance-ids $instanceId --region eu-west-1 --no-paginate
    exit 1
fi
