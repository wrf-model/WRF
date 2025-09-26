#This is to convert several graphics files into a Windows .ico file.
#First, prepare png with transparency at the following resolutions: 16x16, 24x24, 32x32, 48x48, 64x64, 256x256
#Then, prepare the same image sizes but at 8 bit resolution without transparency, saved as bmp
#Then, use the following command to pack the files in one .ico file with a relatively new version of ImageMagic


#ls *.png | xargs -i convert {} -colors 16 16/{}
#ls *.png | xargs -i convert {} -colors 256 256/{}
#ls *.png | xargs -i convert {} -alpha on -channel RGBA -depth 8 rgba/{}
# convert -transparent white icon.svg -alpha on -colors 16 -resize 16x16 16/icon16.png
# convert -transparent white icon.svg -alpha on -colors 16 -resize 24x24 16/icon24.png
# convert -transparent white icon.svg -alpha on -colors 16 -resize 32x32 16/icon32.png
# convert -transparent white icon.svg -alpha on -colors 16 -resize 48x48 16/icon48.png

# convert -transparent white icon.svg -alpha on -colors 256 -resize 16x16 256/icon16.png
# convert -transparent white icon.svg -alpha on -colors 256 -resize 24x24 256/icon24.png
# convert -transparent white icon.svg -alpha on -colors 256 -resize 32x32 256/icon32.png
# convert -transparent white icon.svg -alpha on -colors 256 -resize 48x48 256/icon48.png
#
# convert -transparent white icon.svg -alpha on -channel RGBA -depth 16 -resize 16x16 rgba/icon16.png
# convert -transparent white icon.svg -alpha on -channel RGBA -depth 16 -resize 24x24 rgba/icon24.png
# convert -transparent white icon.svg -alpha on -channel RGBA -depth 16 -resize 32x32 rgba/icon32.png
# convert -transparent white icon.svg -alpha on -channel RGBA -depth 16 -resize 48x48 rgba/icon48.png

#convert icon*.bmp icon*.png icon.ico
convert icon16.png icon24.png icon32.png icon48.png -alpha on -channel RGBA -depth 8 icon.ico
# convert -alpha on rgba/icon*.png 256/icon*.png icon.ico

#see also http://www.iconconstructor.com/tutorials/what_is_icon/