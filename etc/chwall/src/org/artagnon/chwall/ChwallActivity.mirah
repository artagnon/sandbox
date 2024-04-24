package org.artagnon.chwall

import android.app.Activity
import android.content.Intent
import android.content.Context
import android.graphics.BitmapFactory
import android.app.WallpaperManager

class ChwallActivity < Activity
  def onCreate(state)
    super
    setContentView R.layout.main
  end

  $Override
  def onStart
    super
    intent = Intent.new Intent.ACTION_PICK
    intent.setType "image/*"
    startActivityForResult intent, 0
  end

  $Override
  def onActivityResult(requestCode, resultCode, data:Intent)
    super
    selectedImage = data.getData
    imageStream = getContentResolver().openInputStream selectedImage
    thumb = BitmapFactory.decodeStream imageStream
    manager = WallpaperManager.getInstance self
    manager.setBitmap thumb
    finish
  end
end
